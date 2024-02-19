function onChange(e) {
  console.log(e.target.value);
}

function main() {
  console.log("Hello world");
  var elem_range = document.querySelector("input.time-control");
  console.log(elem_range);
  elem_range.addEventListener('input', onChange);
}

if (document.readyState != "complete") {
    window.addEventListener("load", main, { "once": true })
} else {
   main();
}
