(() => {
  const listing = document.getElementById("listing");
  if (!listing) return;

  const buttons = document.querySelectorAll(".sortbtn");
  let sortKey = "name";
  let sortDirection = 1;

  const updateButtons = () => {
    buttons.forEach((button) => {
      const key = button.dataset.sort;
      const label = key === "name" ? "Name" : key === "size" ? "Size" : "Modified";
      button.textContent = key === sortKey
        ? `${label}${sortDirection === 1 ? " ▲" : " ▼"}`
        : label;
    });
  };

  const sortEntries = () => {
    const entries = Array.from(listing.querySelectorAll(".entry"));
    entries.sort((a, b) => {
      const aType = a.dataset.type || "file";
      const bType = b.dataset.type || "file";
      if (aType !== bType) return aType === "dir" ? -1 : 1;

      if (sortKey === "size") {
        return (Number(a.dataset.size || 0) - Number(b.dataset.size || 0)) * sortDirection;
      }
      if (sortKey === "mtime") {
        return (Number(a.dataset.mtime || 0) - Number(b.dataset.mtime || 0)) * sortDirection;
      }
      return (a.dataset.name || "").localeCompare(b.dataset.name || "", undefined, {
        sensitivity: "base",
        numeric: true,
      }) * sortDirection;
    });

    entries.forEach((entry) => listing.appendChild(entry));
    updateButtons();
  };

  buttons.forEach((button) => {
    button.addEventListener("click", () => {
      if (sortKey === button.dataset.sort) {
        sortDirection *= -1;
      } else {
        sortKey = button.dataset.sort;
        sortDirection = 1;
      }
      sortEntries();
    });
  });

  sortEntries();
})();
