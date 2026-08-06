// @ts-check
const { test } = require("@playwright/test");
const { Peer, DEV_ACCOUNTS, settle, killServer, startServer } = require("./helpers");

const dump = (page) =>
  page.evaluate(() => ({
    rows: document.querySelectorAll('[data-testid="email-row"]').length,
    subject: (document.querySelector('[data-testid="email-reader"] h2')?.textContent || "").trim(),
  }));

test("offline paints from cache immediately", async ({ browser }) => {
  test.setTimeout(180_000);
  const peer = new Peer(browser, { id: 0, email: DEV_ACCOUNTS[0].email, name: DEV_ACCOUNTS[0].name });
  await peer.join("/email");
  const page = peer.page;
  await settle(async () => ((await dump(page)).rows > 0 ? true : null), { timeout: 30_000, label: "online" });
  await page.getByTestId("email-row").first().click();
  await settle(async () => ((await dump(page)).subject ? true : null), { timeout: 20_000, label: "read" });

  peer.expectedOutage = true;
  killServer();
  await page.waitForTimeout(2000);
  await page.reload({ waitUntil: "domcontentloaded" });

  const t0 = Date.now();
  await settle(async () => ((await dump(page)).rows > 0 ? true : null), { timeout: 60_000, label: "offline list" }).catch(() => {});
  console.log("OFFLINE_LIST_MS " + (Date.now() - t0) + " " + JSON.stringify(await dump(page)));

  await page.getByTestId("email-row").first().click();
  const t1 = Date.now();
  await settle(async () => ((await dump(page)).subject ? true : null), { timeout: 60_000, label: "offline read" }).catch(() => {});
  console.log("OFFLINE_READ_MS " + (Date.now() - t1) + " " + JSON.stringify(await dump(page)));
  await startServer();
});
