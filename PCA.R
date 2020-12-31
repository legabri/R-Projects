# Load library
library(factoextra)
library(caTools)

# Load dataset
ds = read.csv("/home/gabriel/entrain_valid.csv", header = T)

# Define ID as character
ds_new = ds[1:19]

# Separate numeric and character columns
ds.num = Filter(is.numeric, ds_new)
ds.char = Filter(is.factor, ds_new)

# Separate active columns and rows
ds.act = na.omit(ds.num)

set.seed(123)
split = sample.split(ds.act$emprunt , SplitRatio = 0.7)
ds.active = subset(ds.act, split == TRUE)
ds.test = subset(ds.act, split == FALSE)

# Perform PCA
res.pca = prcomp(ds.active, scale = TRUE)

# Visualize eigenvalues (scree plot). 
# Show the percentage of variances explained by each principal component.
fviz_eig(res.pca)

# Biplot of individuals and variables (It takes time to run, so i take it as a comment)
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
               col.ind = "#696969")  # Individuals color


  # Eigenvalues
eig.val = get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var = get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

# Results for individuals
res.ind = get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 



# Predict the coordinates of new individuals data
ind.sup.coord <- predict(res.pca, newdata = ds.test)
ind.sup.coord[, 1:4]
