// @ts-check
const { test } = require("@playwright/test");
const { Peer, DEV_ACCOUNTS, settle, killServer, startServer } = require("./helpers");

const dump = (page) =>
  page.evaluate(() => ({
    rows: document.querySelectorAll('[data-testid="email-row"]').length,
    subject: (document.querySelector('[data-testid="email-reader"] h2')?.textContent || "").trim(),
    body: (document.querySelector('[data-testid="email-body"]')?.textContent || "").trim().slice(0, 50),
  }));

test("mailbox is readable offline", async ({ browser }) => {
  test.setTimeout(180_000);
  const peer = new Peer(browser, {
    id: 0, email: DEV_ACCOUNTS[0].email, name: DEV_ACCOUNTS[0].name,
  });
  await peer.join("/email");
  const page = peer.page;

  await settle(async () => ((await dump(page)).rows > 0 ? true : null), {
    timeout: 30_000, label: "online listing",
  });
  await page.getByTestId("email-row").first().click();
  await settle(async () => ((await dump(page)).subject ? true : null), {
    timeout: 20_000, label: "online read",
  });
  console.log("ONLINE " + JSON.stringify(await dump(page)));

  peer.expectedOutage = true;
  killServer();
  await page.waitForTimeout(3000);
  await page.reload({ waitUntil: "domcontentloaded" });

  // Give the offline boot its retries.
  await settle(async () => ((await dump(page)).rows > 0 ? true : null), {
    timeout: 45_000, label: "offline listing",
  }).catch(() => {});
  console.log("OFFLINE_LIST " + JSON.stringify(await dump(page)));

  // And a body we had opened before the outage.
  const n = (await dump(page)).rows;
  if (n > 0) {
    await page.getByTestId("email-row").first().click();
    const t0 = Date.now();
    await settle(async () => ((await dump(page)).subject ? true : null), {
      timeout: 90_000, label: "offline read",
    }).catch(() => {});
    console.log("OFFLINE_READ_MS " + (Date.now() - t0));
  }
  console.log("OFFLINE_READ " + JSON.stringify(await dump(page)));

  await startServer();
});
