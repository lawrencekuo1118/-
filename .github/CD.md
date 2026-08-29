# Continuous Deployment — Goddamn SOX

Push to `master` (when `control-design-app/**` changes) runs **CI/CD** via [deploy-goddamn-sox.yml](workflows/deploy-goddamn-sox.yml).

## Pipeline

| Stage | Trigger | Action |
|-------|---------|--------|
| **test** | PR + push to `master` | `Rscript tests/test_assemble.R` |
| **deploy** | push to `master` only (after test passes) | `Rscript deploy.R` → shinyapps.io |
| **verify** | after deploy | HTTP 200 check on live URL |
| **record** | after deploy | Commit updated `goddamn-sox.dcf` bundleId |

Pull requests run **test** only. Merge to `master` runs test then deploy.

Manual deploy: **Actions → CI/CD Goddamn SOX → Run workflow** (uncheck *Tests only* to deploy).

## One-time setup (repo Secrets)

1. Open [shinyapps.io Tokens](https://www.shinyapps.io/admin/#/tokens)
2. Create or copy **token** and **secret**
3. GitHub repo → **Settings → Secrets and variables → Actions → New repository secret**
   - `SHINYAPPS_TOKEN`
   - `SHINYAPPS_SECRET`

Until both secrets exist, CI still passes but the deploy job logs a warning and skips upload.

## Live app

- URL: https://hopesmasher1118.shinyapps.io/goddamn-sox/
- Account: `hopesmasher1118`
- App name: `goddamn-sox`

## Local deploy (optional)

```bash
cd control-design-app
export SHINYAPPS_TOKEN="..."
export SHINYAPPS_SECRET="..."
Rscript deploy.R
```

## Notes

- Commits with message `ci: record goddamn-sox deploy bundle` skip the pipeline (avoids loop from bundle record commits).
