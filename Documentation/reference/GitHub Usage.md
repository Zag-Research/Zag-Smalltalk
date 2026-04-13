This advice came from Gemini. Open to discussion if you have a better idea.

## House rules
We are moving away from forks! From now on, everyone works directly in the main repository. This keeps our history clean and makes collaboration easier. Here is how we do things now:
### 1. The One-Time Setup
If you were previously using a **fork**, you need to point your tools to the main repository. The easiest way is to delete your old local folder and **re-clone** the main repo:
- `git clone https://github.com/Zag-Research/Zag-Smalltalk.git`
### 2. Branch Naming (The "Prefix" Rule)
Don't work directly on `main` (the system won't let you push there anyway!). Create a new branch for every task using these prefixes:
- **`feature/`** — New stuff (e.g., `feature/login-page`)
- **`fix/`** — Fixing bugs (e.g., `fix/header-logo`)
- **`docs/`** — Documentation changes
- **`refactor/`** — Cleaning up code without changing functionality
> **Format:** Use all lowercase and hyphens. No spaces! (e.g., `feature/new-sidebar`) 
 >**Short but Descriptive**: `fix/broken-links` is better than `fix/links` or `fix/the-links-on-the-footer-that-janet-found-yesterday`.

### 3. The Workflow
1. **Pull latest:** Before starting, make sure your `main` is up to date (Click **Fetch/Pull**).
2. **Branch:** Create your branch (e.g., `feature/add-search`)
3. **Work:** Do your thing and commit your changes.
	- Ideally have unit tests (except docs/) and have them all green.
	- Especially if it's a fix - make a failing unit test first, then fix it
4. **Push:** Upload your branch to GitHub.
5. **PR:** Go to GitHub and open a **Pull Request**.
### 4. The "Gatekeeper" Rule
- **Review:** Once your PR is open, tag **[dvmason]** for review.
- **Merge:** Only **[dvmason]** has permission to merge code into `main`, at least for the moment. This ensures someone always has eyes on the final product. When a Pull Request is merged on GitHub, it asks if you want to **"Delete branch."** Dave will say **Yes**  Once a feature is merged into `main`, that branch has done its job. Deleting it keeps the repository from becoming a graveyard of old code. It doesn't delete the history (that’s safe in `main`), it just hides the "construction paper" now that the building is finished.
### 5. Housekeeping (Crucial!)
Once your Pull Request has been merged and the branch is deleted on GitHub:
- **Delete your local copy:** In GitHub Desktop, right-click the branch and select **Delete**. If using CLI, run `git fetch --prune`. In GitHub Desktop if is asks if you want to push the branch or create a PR again, that is (weirdly) that it no longer exists remotely, because it's been merged and deleted.
- **Don't Hoard:** If a branch is merged, it's done. Clear it out to keep your workspace tidy!
### Pro-Tip for everyone:
Every morning when you start work, hit **Fetch/Pull**. It prevents "Merge Conflicts" (Git's version of a headache) by making sure you're always working on the most recent version of the code.

---
## Addressing "issues"
GitHub has a built-in feature that creates the branch for you and even names it after the issue.
1. Open the **Issue** on GitHub.
2. In the right-hand sidebar, under the "Development" section, click **"Create a branch"**.
3. A popup will appear:    
    - It will suggest a name like `123-fix-header-bug`.
        - please add a prefix, either `fix/` or `feature/` to the suggested name
    - Keep the **"Change destination repository"** set to your main repo (not a fork).        
4. Click **Create branch**.
5. **Now, go to your Git tool (GitHub Desktop, CLI, or Zed):**
    - In **GitHub Desktop**: Click "Fetch" and the new branch will appear in your list.        
    - In **CLI**: Run `git fetch` then `git checkout [branch-name]`.

---

[Git Confused Me for Years Until I Found This Simple Guide](https://medium.com/lets-code-future/git-confused-me-for-years-until-i-found-this-simple-guide-a45223bebb40)with a [link to a manual - Master Git in Minutes](https://t3pacademy.gumroad.com/l/Master-git-in-minutes)
