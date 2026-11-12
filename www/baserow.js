// Reads a Baserow table from the browser.
// The token is read from the page and sent only to Baserow. It is never passed to R.

async function baserowGet(base, path, token) {
  let res;
  try {
    res = await fetch(base + path, {
      method: "GET",
      headers: { Authorization: "Token " + token }
    });
  } catch (e) {
    return { ok: false, status: 0, detail: "" };
  }
  let body = null;
  try {
    body = await res.json();
  } catch (e) {
    body = null;
  }
  if (!res.ok) {
    return { ok: false, status: res.status, detail: (body && body.detail) || "" };
  }
  return { ok: true, body: body };
}

async function baserowFetchTable(msg) {
  const tokenBox = document.querySelector("input.baserow-token");
  const token = tokenBox ? tokenBox.value.trim() : "";
  const send = (payload) =>
    Shiny.setInputValue("baserow_payload", JSON.stringify(payload), { priority: "event" });

  if (!token) {
    send({ ok: false, status: -1, detail: "Please enter a token." });
    return;
  }
  const base = msg.url.replace(/\/+$/, "");
  const table = encodeURIComponent(msg.table);

  const fields = await baserowGet(base, "/api/database/fields/table/" + table + "/", token);
  if (!fields.ok) {
    send(fields);
    return;
  }

  let rows = [];
  for (let page = 1; page < 1000; page++) {
    const r = await baserowGet(
      base,
      "/api/database/rows/table/" + table + "/?user_field_names=true&size=200&page=" + page,
      token
    );
    if (!r.ok) {
      send(r);
      return;
    }
    rows = rows.concat(r.body.results || []);
    if (!r.body.next) break;
  }
  send({ ok: true, fields: fields.body, rows: rows });
}

$(document).on("shiny:connected", function () {
  Shiny.addCustomMessageHandler("baserow_fetch", baserowFetchTable);
});

// Download buttons.
// Shiny opens downloads in a new tab. In the browser-only version of the app the
// new tab cannot reach R, so the file never arrives. Instead, fetch the file
// from this page and save it directly.
$(document).on("click", "a.shiny-download-link", async function (event) {
  const href = this.getAttribute("href");
  if (!href) return;
  event.preventDefault();
  const button = this;
  button.classList.add("disabled");
  try {
    const res = await fetch(href);
    if (!res.ok) {
      throw new Error(res.status >= 500 ? "Please click Run first, then try again." : "HTTP " + res.status);
    }
    const blob = await res.blob();
    let name = "download";
    const cd = res.headers.get("content-disposition") || "";
    const match = cd.match(/filename\*?=(?:UTF-8'')?"?([^";]+)"?/i);
    if (match) name = decodeURIComponent(match[1]);
    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = url;
    link.download = name;
    document.body.appendChild(link);
    link.click();
    link.remove();
    setTimeout(() => URL.revokeObjectURL(url), 10000);
  } catch (err) {
    alert("The download did not work. " + err.message);
  } finally {
    button.classList.remove("disabled");
  }
});
