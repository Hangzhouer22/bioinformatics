#step 1, preparing the count data

counts_data <- read.csv()
library(airway)

data(airway)
airway

sample_info <- as.data.frame(colData(airway))
sample_info <- sample_info[,c(2,3)]
sample_info$dex <- gsub('trt', 'treated', sample_info$dex)
sample_info$dex <- gsub('untrt', 'untreated', sample_info$dex)
names(sample_info) <- c('cellLine', 'dexamethasone')
write.table(sample_info, file = "sample_info.csv", sep = ',', col.names = T, row.names = T, quote = F)

countsData <- assay(airway)
write.table(countsData, file = "counts_data.csv", sep = ',', col.names = T, row.names = T, quote = F)

#read in counts data
counts_data <- read.csv('counts_data.csv')
head(counts_data)

#read in sample info
colData <- read.csv('sample_info.csv')
View(colData)

#make sure the row names in colData matche to column names in counts_data
all(colnames(counts_data) %in% rownames(colData))

# are they in the same order?
all(colnames(counts_data) ==rownames(colData))

# Step 2: construct a DeseqDtaSet object......
dds <- DESeqDataSetFromMatrix(countData = counts_data,
                              colData=colData,
                              design = ~dexamethasone)
head(dds)
# prre-filtering: removing rows with low gene counts
#keeping rows that have at least 10 roead total
keep <- rowSums(counts(dds)) >= 10
dds <- dds[keep,]
dds

#set the factor level
dds$dexamethasone <- relevel(dds$dexamethasone, ref = "untreated")
# note: collapse technical replicates
#step 3: Run DESeq
dds <- DESeq(dds)
res <- results(dds)
res
# Explore Results
summary(res)
res0.01 <- results(dds, alpha = 0.01)
summary(res0.01)


#contrasts
resultsNames(dds)
# such as : treated_4hurs, treated_8hrs, untreated
#results(dds, contrast = c("dexamethasone", "treated_4hrs" "treated_8hrs", "untreated"))

#MA plot
plotMA(res)
