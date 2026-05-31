// The client entry. Plain JS for the lean first cut (the Melange/MLX client lands
// in a later iteration). esbuild bundles this via the `fennec build` dune rule.
const btn = document.getElementById("ping");
if (btn) {
  let n = 0;
  btn.addEventListener("click", () => {
    n += 1;
    btn.textContent = `ping ${n}`;
  });
}
console.log("[helloworld] client ready");
