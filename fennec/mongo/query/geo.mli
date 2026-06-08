(** Geospatial predicates for the geo query operators ([$geoWithin], [$geoIntersects], [$near],
    [$nearSphere]). Pure computational geometry over GeoJSON and legacy coordinate pairs — no index
    is needed because Minimongo scans in memory (the 2dsphere index in real MongoDB is only a
    performance structure). Coordinates are [\[longitude, latitude\]] (GeoJSON order). *)

(** A coordinate pair, [(longitude, latitude)] (a.k.a. [(x, y)]). *)
type point = float * float

(** [as_point v] extracts a coordinate pair from a legacy [\[x, y\]] array or a GeoJSON object with
    a [coordinates] field; [None] if [v] is not a coordinate. *)
val as_point : Bson.t -> point option

(** Planar Euclidean distance between two points (legacy [2d] units). *)
val euclid : point -> point -> float

(** Great-circle distance in metres between two [lng/lat] points (WGS84 sphere). *)
val haversine_m : point -> point -> float

(** [within field operand] — [$geoWithin]: is the [field] point inside the operand region? The
    operand is [{$geometry: <GeoJSON Polygon/MultiPolygon>}], [{$box: [[x1,y1],[x2,y2]]}],
    [{$center: [[x,y], r]}] (planar circle), [{$centerSphere: [[lng,lat], radians]}], or
    [{$polygon: [[x,y], …]}]. *)
val within : Bson.t -> Bson.t -> bool

(** [intersects field operand] — [$geoIntersects]: does the [field] geometry intersect the operand's
    [$geometry] (GeoJSON)? Handles point/line/polygon via vertex containment + edge crossing. *)
val intersects : Bson.t -> Bson.t -> bool

(** [near ~force_sphere field operand] — the distance FILTER of [$near]/[$nearSphere]: is the
    [field] point within [\[$minDistance, $maxDistance\]] of the center? GeoJSON ([$geometry]) uses
    spherical metres; legacy points are planar unless [force_sphere] (i.e. [$nearSphere]). Result
    ordering by proximity is a cursor concern, not part of this predicate. *)
val near : force_sphere:bool -> Bson.t -> Bson.t -> bool
