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
