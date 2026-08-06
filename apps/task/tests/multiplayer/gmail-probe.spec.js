// @ts-check
// TEMPORARY: measure cold vs cached folder-listing latency on real Gmail.
const { test } = require("@playwright/test");
const { Peer, DEV_ACCOUNTS, settle } = require("./helpers");

const rows = (page) =>
  page.evaluate(() => document.querySelectorAll('[data-testid="email-row"]').length);

test("gmail listing latency", async ({ browser }) => {
  test.setTimeout(240_000);
  const peer = new Peer(browser, {
    id: 0, email: DEV_ACCOUNTS[0].email, name: DEV_ACCOUNTS[0].name,
  });
  await peer.join("/email");
  const page = peer.page;
  await settle(async () => ((await rows(page)) > 0 ? true : null), {
    timeout: 90_000, label: "first listing",
  });

  const folders = page.getByTestId("email-folder");
  // Switch to another folder and back, timing the return trip — the
  // second visit to INBOX should come from the index.
  const time = async (label, idx) => {
    const t0 = Date.now();
    await folders.nth(idx).click();
    await settle(async () => ((await rows(page)) >= 0 ? true : null), {
      timeout: 60_000, label,
    });
    await page.waitForTimeout(400);
    return Date.now() - t0;
  };
  const away = await time("junk", 3);
  const back1 = await time("inbox cached", 2);
  const away2 = await time("junk again", 3);
  const back2 = await time("inbox cached 2", 2);
  console.log(`TIMING away=${away}ms backCached=${back1}ms away2=${away2}ms backCached2=${back2}ms`);
});
