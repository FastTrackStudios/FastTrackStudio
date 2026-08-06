// @ts-check
// TEMPORARY probe: /email against a seeded maildir, now routed through
// the account-multiplexing backend.
const { test } = require("@playwright/test");
const { Peer, DEV_ACCOUNTS, settle } = require("./helpers");

const dump = (page) =>
  page.evaluate(() => ({
    folders: Array.from(document.querySelectorAll('[data-testid="email-folder"]')).map((b) =>
      (b.textContent || "").replace(/\s+/g, " ").trim(),
    ),
    rows: Array.from(document.querySelectorAll('[data-testid="email-row"]')).map((e) =>
      (e.textContent || "").replace(/\s+/g, " ").trim(),
    ),
    readerSubject: (
      document.querySelector('[data-testid="email-reader"] h2')?.textContent || ""
    ).trim(),
    readerBody: (document.querySelector('[data-testid="email-body"]')?.textContent || "")
      .trim()
      .slice(0, 120),
    errors: Array.from(document.querySelectorAll("[class*='text-destructive']")).map((e) =>
      (e.textContent || "").trim(),
    ),
  }));

test("email still works through the mux", async ({ browser }) => {
  test.setTimeout(120_000);
  const peer = new Peer(browser, {
    id: 0,
    email: DEV_ACCOUNTS[0].email,
    name: DEV_ACCOUNTS[0].name,
  });
  await peer.join("/email");
  const page = peer.page;

  await settle(async () => ((await dump(page)).rows.length > 0 ? true : null), {
    timeout: 30_000,
    label: "envelopes listed",
  });
  console.log("LIST " + JSON.stringify(await dump(page), null, 2));

  await page.getByTestId("email-row").first().click();
  await settle(async () => ((await dump(page)).readerSubject ? true : null), {
    timeout: 20_000,
    label: "reader open",
  });
  const act = (name) =>
    page.getByTestId("email-reader").getByRole("button", { name, exact: true });
  await act("Archive").click();
  await page.waitForTimeout(2000);
  console.log("AFTER " + JSON.stringify(await dump(page), null, 2));
});
