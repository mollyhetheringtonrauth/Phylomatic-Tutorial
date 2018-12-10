# Phylomatic Tutorial
# December 9, 2019
# by Molly Hetherington-Rauth


#####################
### Load packages ###
#####################

require(brranching) # for generating phylogenetic tree via phylomatic; documentation: https://cran.r-project.org/web/packages/brranching/brranching.pdf
requrie(ape) # for the visualization and manipulation of phylogenetic tree; documentation: https://cran.r-project.org/web/packages/ape/ape.pdf
require(phytools)


#######################
### Input Taxa Data ###
#######################

# taxa are stored in a csv file with column 1= Family, and column 2= Species
# I show this option becasue this is likely the way your species data will be stored; however you can type your taxa into a vector in R or you can input a dataset that is already in the phylomatic syntax
taxa <- read.csv('taxa_list.csv', header=T, sep=',', stringsAsFactors=F)
head(taxa) # check data
dim(taxa) # 452 rows, 2 columns

# get data in phylomatic synatax: family/genus/Binomial_name
# example: 'annonaceae/annona/Annona_cherimola'
taxa_list <- c()
for (i in 1:nrow(taxa)) {
	family_name <- tolower(taxa$Family[i]) # converts Family name to lowercase
	split_binomial_name <- strsplit(taxa$Species[i], split=' ')
	genus_name <- tolower(split_binomial_name[[1]][1])
	species_name <- paste(split_binomial_name[[1]][1], split_binomial_name[[1]][2], sep='_')
	phylomatic_syntax <- paste(family_name, genus_name, species_name, sep='/')
	taxa_list <- c(taxa_list, phylomatic_syntax)
}
taxa_list

taxa_subset <- sample(taxa_list, size=20) # i am just randomly sampling 20 taxa from the list in order to make generating the phylogenetic tree quicker



###################################################
### generate phylogenetic tree using phylomatic ###
###################################################

tree <- phylomatic(taxa=taxa_subset, storedtree='zanne2014')
# options for storedtree inlcude: R20120829 (Phylomatic tree R20120829 for plants), smith2011 (Smith 2011, plants), binindaemonds2007 (Bininda-Emonds 2007, mammals), or zanne2014 (Zanne et al. 2014, plants). Default: R20120829 
# note that the function will give a NOTE letting you know which taxa were not found and thus no included in  your phylomatic tree 
# the output is a tree in newick format e.g. ((A,C),B)

plot(tree) # you can quickly inspect the tree

write.tree(tree, 'newick_tree.tr')
###########################################
### Tree Visualization and Manipulation ###
###########################################

# Note that is you used the online Phylomatic platform you can copy and paste the newick tree that pops up in a new browser window for the next steps just use and note the semicolor at the end:

# tree <- read.tree(text='((((eriogonum_fasciculatum:89.4609,Silene_antirrhina:89.4609)Caryophyllales:27.2363,((((bidens_hillebrandiana:22.8719,bidens_cervicata:22.8719)bidens:22.8719,erigeron_rosulatus:45.7437,hazardia_rosarica:45.7437,Hazardia_squarrosa:45.7437)Asteraceae:57.1278,(((bertiera_bracteosa:56.8699,galium_ovalleanum:56.8699)Rubiaceae:16.4995,((diplacus_calycinus:38.1329,verbena_sedula:38.1329):3.88331,justicia_glaziovii:42.0162):31.3533):1.62956,Phacelia_ramosissima:74.999):27.8725):4.73798,Arctostaphylos_cruzensis:107.609)Asteridae:9.08781)Pentapetalae:2.46823,(helianthemum_patens:117.535,lotus_nevadensis:117.535)Rosidae:1.63077)Gunneridae:17.735,(berberis_corymbosa:117.268,Berberis_microphylla:117.268)Berberidaceae:19.6321);') 


### Adding missing taxa ###
###########################

# The phylomatic function let you know which taxa were not included in the phylogeny however you can also get the list of taxa that are not included in the phyloeny using a bit of code.
# first we need to fix the fact taht for some reason the first letter of the binomial name of soem of the taxa is not captialized


tree.tips <- tree$tip.label # the taxa that are included in the tree
# so I don't know why but for some reason the first letter of the binomial name of some taxa is not cpatialized so we need to fix that...
corrected.tips <- c()
for (i in 1:length(tree.tips)) {
	
	string <- strsplit(tree.tips[i], split='')
	first_letter <- toupper(string[[1]][1])
	corrected.i <- sub('^|[[:alpha:]]', first_letter, tree.tips[i])
	corrected.tips <- c(corrected.tips, corrected.i)
}

tree$tip.label <- corrected.tips  # assigns corrected tip labels to the phylogeny
tree.tips <- tree$tip.label # corrected tip labels

index <- which(!(taxa$Species %in% tree.tips)) # identifies which taxa are not included in the phylomatic tree

missing_taxa <- taxa$Species[index]

# next idenify which taxa you want the new sister species to be for each of the missing taxa
missing_taxa_i <- 
sister <- 

sister_location <- which(tree$tip.label==sister) 
position <- tree$edge.length[which(tree$edge[,2])]==which(tree$tip.label == sister) # here I set the edge.length (branch length) to equal the branch length of the sister taxa. You can change this by multipying position by a fraction. For example if you mulitple position by 0.5 then the taxa will be place halway along the branch of the sister taxa. 
edge.length <- tree$edge.length[which(tree$edge[,2])]==which(tree$tip.label==sister) 

tree <- bind.tip(tree, to_drop, where= where, position=position, edge.length=edge.length) # the final argument edge.length specifies the terminal branch lenght and must be included when the phylogeny is not ultrametric



### Correct misplaced taxa ###
##############################
# You will need to manually create a list of taxa that are misplaced by visually inspecting the tree
# We will drop those tips and then add them as sister taxa in the correct location
# to do this I followed the tutorial and modified code by Liam Revell which can be found here: http://blog.phytools.org/2015/08/adding-tip-to-sister-taxon-on-tree.html

to_drop <- 
sister <-

tree <- drop.tip(tree, to_drop) # remove taxa form phylogeny
sister_location <- which(tree$tip.label==sister) 
position <- tree$edge.length[which(tree$edge[,2])]==which(tree$tip.label == sister) # here I set the edge.length (branch length) to equal the branch length of the sister taxa. You can change this by multipying position by a fraction. For example if you mulitple position by 0.5 then the taxa will be place halway along the branch of the sister taxa. 
edge.length <- tree$edge.length[which(tree$edge[,2])]==which(tree$tip.label==sister) 

tree <- bind.tip(tree, to_drop, where= where, position=position, edge.length=edge.length) # the final argument edge.length specifies the terminal branch lenght and must be included when the phylogeny is not ultrametric


### Now replot the phylogeny and see if everything looks good ###
#################################################################
plot(tree) # you can quickly inspect the tree
plot(tree, type='f'cex=0.3) # for larger phylogenies it may be helpful to plot a circular tree (type='f') and descrease the font size (cex=0.3)









 








