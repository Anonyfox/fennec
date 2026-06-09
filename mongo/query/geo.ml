(* Geospatial predicates for the geo query operators ($geoWithin, $geoIntersects, $near,
   $nearSphere). Pure computational geometry over GeoJSON and legacy coordinate pairs — no index is
   needed because Minimongo is an in-memory brute-force scan (the 2dsphere index in real MongoDB is
   only a performance structure). Coordinates are [longitude, latitude] (GeoJSON order). *)

open Bson

type point = float * float (* (lng/x, lat/y) *)

type geom =
  | Pt of point
  | Line of point list
  | Poly of point list list (* rings: exterior first, then holes *)
  | Multi of geom list

let earth_radius_m = 6378137.0 (* WGS84 equatorial, as MongoDB uses *)
let deg2rad d = d *. Float.pi /. 180.

(* a coordinate pair from a Bson value: legacy [x, y] or a GeoJSON object with "coordinates" *)
let as_point v =
  let pair a b = match (Bson.as_float a, Bson.as_float b) with Some x, Some y -> Some (x, y) | _ -> None in
  match v with
  | Array [ a; b ] -> pair a b
  | Document _ -> ( match Bson.get v "coordinates" with Some (Array [ a; b ]) -> pair a b | _ -> None)
  | _ -> None

let as_ring = function Array pts -> List.filter_map as_point pts | _ -> []
let as_rings = function Array rings -> List.map as_ring rings | _ -> []

(* parse a GeoJSON geometry (or a bare legacy point) *)
let rec parse (v : Bson.t) : geom option =
  match (Bson.get_string v "type", Bson.get v "coordinates") with
  | Some "Point", Some c -> ( match as_point c with Some p -> Some (Pt p) | None -> None)
  | Some "LineString", Some c -> Some (Line (as_ring c))
  | Some "Polygon", Some c -> Some (Poly (as_rings c))
  | Some "MultiPoint", Some c -> Some (Multi (List.map (fun p -> Pt p) (as_ring c)))
  | Some "MultiLineString", Some (Array ls) -> Some (Multi (List.map (fun l -> Line (as_ring l)) ls))
  | Some "MultiPolygon", Some (Array ps) -> Some (Multi (List.map (fun p -> Poly (as_rings p)) ps))
  | Some "GeometryCollection", _ -> (
      match Bson.get v "geometries" with Some (Array gs) -> Some (Multi (List.filter_map parse gs)) | _ -> None)
  | _ -> ( match as_point v with Some p -> Some (Pt p) | None -> None)

(* ---- distances ---- *)

let euclid (x1, y1) (x2, y2) = sqrt (((x1 -. x2) ** 2.) +. ((y1 -. y2) ** 2.))

(* great-circle central angle (radians) between two lng/lat points *)
let central_angle (lng1, lat1) (lng2, lat2) =
  let dlat = deg2rad (lat2 -. lat1) and dlng = deg2rad (lng2 -. lng1) in
  let a =
    (sin (dlat /. 2.) ** 2.)
    +. (cos (deg2rad lat1) *. cos (deg2rad lat2) *. (sin (dlng /. 2.) ** 2.))
  in
  (* clamp: rounding can push [a] just past 1.0 for near-antipodal points, which would make
     [sqrt (1. -. a)] nan and silently drop the match *)
  let a = Float.min 1.0 (Float.max 0.0 a) in
  2. *. atan2 (sqrt a) (sqrt (1. -. a))

(* parse cache for the (constant) query geometry — the matcher re-enters per document with the same
   operand, so without this the same polygon would be re-parsed once per document scanned *)
let parse_cache : (string, geom option) Hashtbl.t = Hashtbl.create 16

let parse_memo v =
  let k = Bson.to_string v in
  match Hashtbl.find_opt parse_cache k with
  | Some r -> r
  | None ->
      let r = parse v in
      Hashtbl.replace parse_cache k r;
      r

let rec all_points = function
  | Pt p -> [ p ]
  | Line ps -> ps
  | Poly (ext :: _) -> ext
  | Poly [] -> []
  | Multi gs -> List.concat_map all_points gs

let envelope = function
  | [] -> None
  | (x0, y0) :: _ as pts ->
      Some (List.fold_left (fun (a, b, c, d) (x, y) -> (min a x, min b y, max c x, max d y)) (x0, y0, x0, y0) pts)

(* cheap axis-aligned bounding-box overlap test, to reject obviously-disjoint geometries before the
   O(edges²) segment-intersection work *)
let bbox_overlap a b =
  match (envelope (all_points a), envelope (all_points b)) with
  | Some (ax0, ay0, ax1, ay1), Some (bx0, by0, bx1, by1) ->
      not (ax1 < bx0 || bx1 < ax0 || ay1 < by0 || by1 < ay0)
  | _ -> true

let haversine_m p q = earth_radius_m *. central_angle p q

(* ---- point in polygon (ray casting) ---- *)

let point_in_ring (px, py) ring =
  let arr = Array.of_list ring in
  let n = Array.length arr in
  if n < 3 then false
  else begin
    let inside = ref false in
    let j = ref (n - 1) in
    for i = 0 to n - 1 do
      let xi, yi = arr.(i) and xj, yj = arr.(!j) in
      if (yi > py) <> (yj > py) && px < ((xj -. xi) *. (py -. yi) /. (yj -. yi)) +. xi then
        inside := not !inside;
      j := i
    done;
    !inside
  end

let point_in_poly pt = function
  | [] -> false
  | ext :: holes -> point_in_ring pt ext && not (List.exists (fun h -> point_in_ring pt h) holes)

let rec point_in_geom pt = function
  | Poly rings -> point_in_poly pt rings
  | Multi gs -> List.exists (point_in_geom pt) gs
  | Pt p -> p = pt
  | Line _ -> false

(* ---- segment intersection (for $geoIntersects on lines/polygons) ---- *)

let orient (ax, ay) (bx, by) (cx, cy) =
  let v = ((bx -. ax) *. (cy -. ay)) -. ((by -. ay) *. (cx -. ax)) in
  if v > 0. then 1 else if v < 0. then -1 else 0

let seg_cross (p1, p2) (p3, p4) =
  orient p1 p2 p3 <> orient p1 p2 p4 && orient p3 p4 p1 <> orient p3 p4 p2

let edges_of = function
  | Line pts | Poly (pts :: _) ->
      let rec pairs = function a :: (b :: _ as tl) -> (a, b) :: pairs tl | _ -> [] in
      pairs pts
  | _ -> []

let vertices_of = function
  | Pt p -> [ p ]
  | Line pts -> pts
  | Poly (ext :: _) -> ext
  | Poly [] -> []
  | Multi _ -> []

let rec geom_intersects a b =
  match (a, b) with
  | Pt p, _ -> point_in_geom p b || b = Pt p
  | _, Pt p -> point_in_geom p a
  | Multi gs, _ -> List.exists (fun g -> geom_intersects g b) gs
  | _, Multi gs -> List.exists (geom_intersects a) gs
  | _ ->
      bbox_overlap a b
      && (let ea = edges_of a and eb = edges_of b in
          List.exists (fun s1 -> List.exists (fun s2 -> seg_cross s1 s2) eb) ea
          || List.exists (fun v -> point_in_geom v b) (vertices_of a)
          || List.exists (fun v -> point_in_geom v a) (vertices_of b))

(* ---- the operators ---- *)

(* $geoWithin: the field point is inside the operand region *)
let within (fv : Bson.t) (operand : Bson.t) : bool =
  match (as_point fv, operand) with
  | Some pt, Document kvs -> (
      let get k = List.assoc_opt k kvs in
      let circle r kind cp = match Bson.as_float r with Some rad -> kind pt cp <= rad | None -> false in
      match get "$geometry" with
      | Some g -> ( match parse_memo g with Some geom -> point_in_geom pt geom | None -> false)
      | None -> (
          match get "$box" with
          | Some (Array [ a; b ]) -> (
              match (as_point a, as_point b) with
              | Some (x1, y1), Some (x2, y2) ->
                  let px, py = pt in
                  px >= min x1 x2 && px <= max x1 x2 && py >= min y1 y2 && py <= max y1 y2
              | _ -> false)
          | _ -> (
              match get "$center" with
              | Some (Array [ c; r ]) -> ( match as_point c with Some cp -> circle r euclid cp | None -> false)
              | _ -> (
                  match get "$centerSphere" with
                  | Some (Array [ c; r ]) -> (
                      match as_point c with Some cp -> circle r central_angle cp | None -> false)
                  | _ -> (
                      match get "$polygon" with
                      | Some (Array pts) -> point_in_ring pt (List.filter_map as_point pts)
                      | _ -> false)))))
  | _ -> false

(* $geoIntersects: the field geometry intersects the operand's $geometry *)
let intersects (fv : Bson.t) (operand : Bson.t) : bool =
  match operand with
  | Document kvs -> (
      match List.assoc_opt "$geometry" kvs with
      | Some g -> (
          match (parse_memo g, parse fv) with Some qg, Some fg -> geom_intersects fg qg | _ -> false)
      | None -> false)
  | _ -> false

(* $near / $nearSphere: the field point is within [$minDistance, $maxDistance] of the center.
   GeoJSON ($geometry) uses spherical metres; legacy points are planar unless [force_sphere]. Note:
   this is the distance FILTER only — result ordering by proximity is a cursor concern. *)
let near ~force_sphere (fv : Bson.t) (operand : Bson.t) : bool =
  match (as_point fv, operand) with
  | Some pt, Document kvs -> (
      let geojson, center =
        match List.assoc_opt "$geometry" kvs with
        | Some g -> (true, as_point g)
        | None -> (false, match List.assoc_opt "$near" kvs with Some c -> as_point c | None -> None)
      in
      match center with
      | None -> false
      | Some c ->
          let sphere = force_sphere || geojson in
          let dist = if sphere then haversine_m pt c else euclid pt c in
          let within_max = match List.assoc_opt "$maxDistance" kvs with Some v -> ( match Bson.as_float v with Some m -> dist <= m | None -> true) | None -> true in
          let within_min = match List.assoc_opt "$minDistance" kvs with Some v -> ( match Bson.as_float v with Some m -> dist >= m | None -> true) | None -> true in
          within_max && within_min)
  | _ -> false
