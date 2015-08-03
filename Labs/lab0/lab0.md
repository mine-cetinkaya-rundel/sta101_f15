---
title: "Lab 0 - Introduction"
output:
  html_document:
    css: ../lab.css
    highlight: pygments
    theme: cerulean
  pdf_document: default
---

### (1) Familiarize yourself with the course website

The course website is at https://stat.duke.edu/courses/Fall14/sta101.001. This is where all course materials will be posted.

### (2) Log on to RStudio

You will be using R and RStudio throughout the course both to learn the statistical concepts discussed in the texbook and also to analyze real data and come to informed conclusions.  To straighten out which is which: R is the name of the programming language itself and RStudio is a convenient interface.

The easiest way to access RStudio is to click on the RStudio link on the upper left corner of the course website. Alternatively you can directly go to https://vm-manage.oit.duke.edu/containers. Then, click on the link that says *Click here to log in to your R-Studio environment*, and log on using your NetID and password.

In the next lab you will learn about the fundamental building blocks of R and RStudio, but for now we just want to make sure that you can log on successfully and run *some* code.

The RStudio window should look something like this:

![rinterface](rInterface2014.png)

The panel in the upper right contains your *workspace* as well as a history of the commands that you've previously entered.  Any plots that you generate will show up in the panel in the lower right corner.

The panel on the left is where the action happens.  It's called the *console*.  Everytime you launch RStudio, it will have the same text at the top of the console telling you the version of R that you're running.  Below that information is the *prompt*.  As its name suggests, this prompt is really a request, a request for a command.  Initially, interacting with R is all about typing commands and interpreting the output. These commands and their syntax have evolved over decades (literally) and now provide what many users feel is a fairly natural way to access data and organize, describe, and invoke statistical computations.

You can use R as a calculator. To get you started, enter the following command at the R prompt (i.e. right after `>` on the console).  You can either type it in manually or copy and paste it from this document.


```r
2 + 2
```

```
## [1] 4
```


And you can save this result to an object that you can access later


```r
x = 2 + 2
```


Try typing `x` in the console to verify that.

You can also see this new object in your environment on the upper right pane. Next time you log on to RStudio the object will still be here.

Throughout the semester you will learn about how to use R to do data analysis, and in the meantime you will be exposed to some programming. In addition, you will learn best practices for saving your code and making sure that your analysis is reproducible.

### (3) Familiarize yourself with Sakai

While all course materials will be distributed via the course website listed above, we will also use Sakai turning in assignments, grades, and forum discussion. Go to [Sakai](https://sakai.duke.edu) and log on (using your NetID and password). If you are enrolled in the course it should be listed among your courses for this semester.

### (4) Sign up for Piazza

While you're on the Sakai page for this course, sign up for Piazza, which is the platform that we will use for online discussion related to this course. Click on the Piazza link on the course site, complete the enrollment, and make sure that you're able to view and post.

### (5) Questions for class survey

One of your first tasks in this class is to help design a survey. This survey will be completed anonymously. It will (ideally) have information on variables you are interested in working with. When writing a question think about whether you would feel comfortable answering it on an anonymous survey.

Work with classmates in your row to come up with a survey question, and add it to this [Google doc](https://docs.google.com/document/d/1LwG5QTkrcW-q-BvcDS4fUToWatDs_eaeLMlzOU8xEuc/edit?usp=sharing). Make sure that the wording of the question is clear, and (if categorical) the answer choices make sense.

Before adding a question check to make sure that it hasnâ