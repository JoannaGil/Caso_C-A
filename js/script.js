

document.addEventListener("DOMContentLoaded", function () {
  function openSectionFromHash() {
    var id = window.location.hash.replace("#", "");
    if (!id) return;
    var anchor = document.getElementById(id);
    if (!anchor) return;
    var details = anchor.nextElementSibling;
    if (details && details.tagName && details.tagName.toLowerCase() === "details") {
      details.open = true;
    }
  }

  document.querySelectorAll(".sidebar-nav a").forEach(function (link) {
    link.addEventListener("click", function () {
      setTimeout(openSectionFromHash, 0);
    });
  });

  openSectionFromHash();
  
});