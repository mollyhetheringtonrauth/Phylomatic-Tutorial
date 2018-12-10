# Phylomatic Tutorial

This respository xxx tutorial posted on my blog (www.mollyhetheringtonrauth.com) intended to walk you through how to generate and modify a phylogenetic tree using Phylomatic and R. the respository contains three files: an R script, a taxa list, and a newick format tree. 

## Background
Continuing on with my phylogenetics theme, I wanted to write a tutorial for how to create a supertree using Phylomatic. I recently used phylomatic to generate a figure to highlight the diversity of plant taxa included in my study. I had trouble finding a tutorial on how to use it and fiddled around for an hour or so before I got the hang of it, so here I decided to write up what I learned hoping it will say someone time in the future.

The original paper describing Phylomatic by Webb & Donoghue, 2004 can be found here and the online platform can be found here. Phylomatic was originally developed as a tool to easily make phylogenies using community data. These phylogenies could then be used to generate community phylogenetic measures (see Webb et al, 2002). Originally Phylomatic was restricted to seed plants or else required a reference phylogeny provided by the user, but now Phylomatic has expanded to include an animal reference phylogeny as well as multiple reference phylogenies for seed plants.

Essentially, Phylomatic generates a phylogenetic tree of a list of taxa that is provided by the user, using either one of several ‘built in’ phylogenies that are well cited in the literature (e.g. Zanne et al., 2014) or a phylogeny that is provided by the user. It’s pretty much that easy. However after viewing the tree you may notice that some tips are out of place. For example, you may notice that taxa of the same genus do not grouped together, but rather form paraphyletic groups. It is possible to correct this in R.

In this tutorial I show you how to generate a phylogeny using Phylomatic in R. Then I show you how to add and re-arrange tips in the phylogeny in R. I use R for the focus of this tutorial so I don’t have to change between environments; however it is also possible to use Phylomatic from the terminal. To do so you will need to install the Phylocom software from Github (here) which is used for the analysis of community ecology data.  A full script version of this tutorial, with lots of detailed annotation, can be found on my Github, here.
