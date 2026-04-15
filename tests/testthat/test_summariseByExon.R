context("Exon quantification")

test_that("summariseByExon runs on bundled SE and returns a well-formed SE", {
    se <- readRDS(system.file("extdata",
        "seOutput_SGNex_A549_directRNA_replicate5_run1_chr9_1_1000000.rds",
        package = "bambu"))

    seExon <- summariseByExon(se)

    expect_s4_class(seExon, "RangedSummarizedExperiment")
    expect_true("counts" %in% assayNames(seExon))
    expect_equal(ncol(seExon), ncol(se))
    expect_equal(colnames(seExon), colnames(se))
    expect_true("GENEID" %in% colnames(mcols(rowRanges(seExon))))
    # rownames are unique chr:start:end:strand keys
    expect_equal(length(unique(rownames(seExon))), nrow(seExon))
})

test_that("summariseByExon aggregation identity holds with synthetic counts", {
    se <- readRDS(system.file("extdata",
        "seOutput_SGNex_A549_directRNA_replicate5_run1_chr9_1_1000000.rds",
        package = "bambu"))

    # Rebuild SE with two samples of Poisson counts (bundled fixture is all zero)
    set.seed(1)
    n <- nrow(se)
    cnt <- Matrix::Matrix(cbind(
        sampleA = as.numeric(rpois(n, 5)),
        sampleB = as.numeric(rpois(n, 10))
    ), sparse = TRUE)
    rownames(cnt) <- rownames(se)
    rr <- rowRanges(se)
    mcols(rr) <- rowData(se)
    se2 <- SummarizedExperiment(
        assays    = S4Vectors::SimpleList(counts = cnt),
        rowRanges = rr,
        colData   = DataFrame(id = c("sampleA", "sampleB"),
                              row.names = c("sampleA", "sampleB")))

    seExon <- summariseByExon(se2)

    # Per-sample identity: colSums(exon) == sum_tx #exons(tx) * count(tx)
    exonPerTx <- lengths(rowRanges(se2))
    expected <- colSums(as.matrix(cnt) * exonPerTx)
    observed <- colSums(as.matrix(assays(seExon)$counts))
    expect_equal(unname(observed), unname(expected))

    # Spot-check the most-shared exon against a manual tx-set sum
    ex  <- unlist(rowRanges(se2), use.names = TRUE)
    key <- paste(as.character(seqnames(ex)), start(ex), end(ex),
                 as.character(strand(ex)), sep = ":")
    top <- names(sort(table(key), decreasing = TRUE))[1]
    txs <- unique(names(ex)[key == top])
    manual <- colSums(as.matrix(cnt[txs, , drop = FALSE]))
    expect_equal(unname(as.numeric(assays(seExon)$counts[top, ])),
                 unname(manual))
})
