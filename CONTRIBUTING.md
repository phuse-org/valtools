
# How you can contribute

Thanks for your interest in contributing to valtools! There are several ways you can contribute to this package:

* **Propose an idea.** Do you have an idea for a new valtools feature? Take a look at the issue list first to see if it isn't included or suggested yet. If not, suggest your idea as an issue on GitHub.

* **Report a bug.** Report a bug as an issue on GitHub so we can fix it. When filing an issue, the most important thing is to include a minimal reproducible example so that we can quickly verify the problem, and then figure out how to fix it. There are three things you need to include to make your example reproducible: required packages, data, code.

    1. Packages should loaded at the top of the script.
    2. The easiest way to include data is to use `dput()` to generate the R code to recreate it. But even better is if you can create a data.frame() with just a handful of rows and columns that still illustrates the problem.
    3. Spend a little bit of time ensuring that your code is easy for others to read: (1) Make sure you've used spaces and your variable names are concise, but informative. (2) Use comments to indicate where your problem lies. (3) Do your best to remove everything that isn't related to the problem. The shorter your code is, the easier it is to understand.
<br><br>

* **Improve documentation.** Noticed a typo? Think a function could use a better example? Improvements to the documentation is very welcome!

* **Contribute code.** If you'd like to fix a bug or implement a new feature, have a look at the issue list and leave a comment on things you want to work on. If you have a new idea, first file an issue and make sure someone from the our team agrees that it’s a problem, and is happy with your basic proposal for fixing it. Also see the development guidelines below.


# Development guidelines

We follow the [Gitflow](https://nvie.com/posts/a-successful-git-branching-model/) development model and the guidelines from [R packages](https://r-pkgs.org/) by Hadley Wickham and Jenny Bryan.

1. Create a branch in git and make your changes.

    * Write your code.
    * Restart R Session Cmd+Shift+F10 (Ctrl+Shift+F10 for Windows)
    * Build and Reload Cmd+Shift+B (Ctrl+Shift+B for Windows)
    * Test Package Cmd+Shift+T (Ctrl+Shift+T for Windows)
    * Document Package Cmd+Shift+D (Ctrl+Shift+D for Windows)
    * Check Package Cmd+Shift+E (Ctrl+Shift+E for Windows)
<br><br>

2. Push branch to github and issue a pull request (PR).

3. Discuss the pull request.

4. Iterate until the maintainer accepts the PR or decides it's not a good fit for valtools.

This might feel overwhelming the first time you get set up, but it gets easier with practice.

## Before every commit

Before you commit, run the following commands one more time to make sure you didn’t break anything.

* Restart R Session Cmd+Shift+F10 (Ctrl+Shift+F10 for Windows)
* Check Package Cmd+Shift+E (Ctrl+Shift+E for Windows)
* Document Package Cmd+Shift+D (Ctrl+Shift+D for Windows)

## Pull requests

Your pull request should follow these guidelines:

1. **Motivation for changes.** Your pull request should clearly and concisely motivate the need for change.
2. **Only related changes.** Before you submit your pull request, please check to make sure that you haven't accidentally included any unrelated changes. These make it harder to see exactly what's changed, and to evaluate any unexpected side effects. Each PR corresponds to a git branch, so if you expect to submit multiple changes make sure to create multiple branches.

3. **Coding style.** Please follow the official [tidyverse style](https://style.tidyverse.org/). Maintaining a consistent style across the whole code base makes it much easier to jump into the code. If you're modifying existing valtools code that doesn't follow the style guide, a separate pull request to fix the style would be greatly appreciated.

4. **Documentation.** If you're adding new parameters or a new function, you'll also need to document them with roxygen. Make sure to re-run `devtools::document()` on the code before submitting.

5. If fixing a bug or adding a new feature, please add a testthat unit test.

# Code of conduct

Please note that valtools is released with a [Contributor Code of Conduct](CONDUCT.md). By contributing to this project,
you agree to abide by its terms.

# Learn about package development

Before contributing, you may want to read a bit more about package development in general.

* [Package development workflow with the usethis package by Emil Hvitfeldt](https://www.hvitfeldt.me/blog/usethis-workflow-for-package-development/). (blog post)

* [Writing an R package from scratch by Hilary Parker](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/) or its [updated version by Tomas Westlake](https://r-mageddon.netlify.com/post/writing-an-r-package-from-scratch/) that shows how to do the same more efficiently using the usethis package. (blog post)

* [You can create an R package in 20 minutes by Jim Hester](https://resources.rstudio.com/rstudio-conf-2018/you-can-make-a-package-in-20-minutes-jim-hester). (conference talk)

* [R packages by Hadley Wickham and Jenny Bryan](https://r-pkgs.org/). (book)

* [Building Tidy Tools](https://blog.rstudio.com/2019/02/06/rstudio-conf-2019-workshops/). (rstudio::conf workshop)

This contributing guide was inspired by and modified from the [ggplot2 contributing.md](ahttps://github.com/tidyverse/ggplot2/blob/master/CONTRIBUTING.md), [rOpenSci's contributing guide](https://devguide.ropensci.org/contributingguide.html), and [this template](https://gist.github.com/peterdesmet/e90a1b0dc17af6c12daf6e8b2f044e7c).
