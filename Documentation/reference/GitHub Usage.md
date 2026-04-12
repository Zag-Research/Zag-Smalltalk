This advice came from Gemini. Open to discussion if you have a better idea.

All members of the team can create branches. The process is:
1. create a branch. Start every branch name with a category followed by a slash. Most professional teams use these four:
	- **`feature/`** : New functionality (e.g., `feature/search-bar`).
    - **`fix/`** or **`bugfix/`** : Fixing something that’s broken (e.g., `fix/header-alignment`).
    - **`docs/`** : Changes only to documentation or README files.
    - **`refactor/`** : Cleaning up code without changing how it works.
2. Simple Formatting Rules. To keep things clean across different operating systems (Windows and Mac handle capital letters differently in file names, which can cause Git "ghost" bugs), suggest these ground rules:
	- **Use Kebab-case:** Use hyphens to separate words (`feature/add-login`), not spaces or underscores.
    - **All Lowercase:** Avoid `Feature/Add-Login`. Stick to `feature/add-login`.
    - **Short but Descriptive:** `fix/broken-links` is better than `fix/links` or `fix/the-links-on-the-footer-that-janet-found-yesterday`.
3. work on the branch. Ideally have unit tests (except docs/) and have them all green
4. create a pull request
5. pull request will be reviewed, merged, and the branch deleted. Currently only Dave can merge pull requests. When you merge a Pull Request on GitHub, it will ask if you want to **"Delete branch."** * **Say Yes.** * Once a feature is merged into `main`, that branch has done its job. Deleting it keeps your repository from becoming a graveyard of old code. It doesn't delete the history (that’s safe in `main`), it just hides the "construction paper" now that the building is finished.
6. Example names: Good vs. Bad

| Type            | Good Example           | Bad Example  | Why?                                 |
| --------------- | ---------------------- | ------------ | ------------------------------------ |
| **New Feature** | `feature/user-profile` | `new_page`   | Too vague; uses underscores.         |
| **Bug Fix**     | `fix/dark-mode-toggle` | `FIX-IT-NOW` | All caps is aggressive; no category. |
| **Misc**        | `docs/update-readme`   | `my-work`    | Who is "my"? What is the "work"?     |

[Git Confused Me for Years Until I Found This Simple Guide](https://medium.com/lets-code-future/git-confused-me-for-years-until-i-found-this-simple-guide-a45223bebb40)with a [link to a manual - Master Git in Minutes](https://t3pacademy.gumroad.com/l/Master-git-in-minutes)
