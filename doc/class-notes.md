## Notes 2016 11 09

We can have a tie in the rankings. For example 3 words.

* We could have r = (1,2,3)
* If we believe there is a tie, we could do r = (1,1,3)
* BUT We will have a scaling difference with Wilcoxon Rank Sum.
* Therefore, we must output r = (1.5, 1.5, 3)
    + The 1.5 comes from averaging 1 and 2.

To solve this once you calculate likelihood, **use rank() function in R, to generate ranking**

