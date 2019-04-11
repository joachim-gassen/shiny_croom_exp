
A Shiny-based Classroom Experiment with Real-Time Results Presentation
======================================================================

Overview
--------

This is the repository for a simple classroom experiment that I used in the first class of my introductory cost accounting course. It might be helpful to construct similar experiments and, of course, if someone is interested in replicating the results from our run in her or his own class.

The experiment tests whether cost allocation (variable cost or full cost) affects pricing decisions in a simple one product pricing setting.

This is the experimental treatment with variable cost information.

<img src="pics/experiment_variable_cost.png" width="90%" style="display: block; margin: auto;" />

And this is the treatment with full cost information.

<img src="pics/experiment_full_cost.png" width="90%" style="display: block; margin: auto;" />

In principle, both formats provide the same underlying information and thus, pricing should not differ across the two treatments if subjects rationally extract all available information.

Under the full cost treatment, however, fix costs are being allocated to units based on expected unit sales. Assuming that subjects use simple "cost plus" heuristics when setting their prices, this could imply that they are less likely to set prices below $12 when receiving the full cost treatment. Note that setting prices below $12 might very well be rational when demand reacts strongly to price as the increased unit sales can compensate for the loss in margin.

These are the results from our experimental run as presented by the real-time shiny result app:

<img src="pics/results_app.png" width="90%" style="display: block; margin: auto;" />

You can assess the [online version of our results here](https://trr266.wiwi.hu-berlin.de/shiny/kore_croom_eval). At least for our students, it seems as if, in line with some prior literature, full cost accounting causes them to set higher prices. In real life, this might imply the risk of loosing market share, eventually pricing oneself out of the market.

Replicating the analysis and reusing the code
---------------------------------------------

The repository contains the data from our analysis as well as all the code that you need to set up the experiment on your own shiny server. I have included the English language version along with the original German language version that I ran in class.

If you want to re-run the exact same experiment in your class for didactcial purposes and/or to replicate our findings, you can also simply reach out and I can provide you access to our shiny server so that you can run the experiment with your students.

If you want to use the code as a starting point for your own classroom experiments, here are some remarks about the code.

-   The repository contains both shiny apps (`app_experiment_en.R` and `app_restults_en.R`) in one directory. Make sure to export them as two separate shiny apps in separate folders.
-   The file `kore_croom_exp.sqlite3` contains an empty `SQLite` database to store the response data. The file `ìnit_sqlite_db.R` contains some code snippets that might be useful when working with the database.
-   When you want to use the real-time results feature, your result app needs to have access to the database file that the experiment app is writing to. When you are hosting this on your own shiny server this can be realized by the results app linking to the database file in the folder of the experiment app. If you plan to host your apps on a service like 'shinyapps.io' then this will most likely not be feasible. In this case, you might consider switching to an external database.
-   When running this in a large class, your students might experience "Too many users" errors from shiny as shiny has a limit of 100 concurrent users for a given app. When running your own shiny server you can configure shiny to allow more users but my guess is that you will run into performance issues at some point. Another option (the one that I went with) is to ask students to be patient and hit 'reload'.
-   Finally, I whipped all this up relatively quickly, so I cannot guarantee that the code is error free and runs exactly as advertised. Please get in touch when you encounter something odd.

This is it. Let me know your thoughts and I would be very happy to get in touch if you a reusing the code for your own projects. In particular, I would be interested in hearing about people that replicate our experimental classroom result.

Enjoy!
