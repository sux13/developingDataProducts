library(shiny)
library(ggplot2)
library(reshape2)
require(googleVis)
shinyServer(function(input, output) {

    pop<- reactive({sample(1:20, input$population, replace = TRUE)})
    bootstrapSample<-reactive({sample(pop(),input$sampleSize*input$numSample,replace = TRUE)})

    popVar<- reactive({round(var(pop()),2)})

    output$population <- renderText({input$population})
    output$numSample <- renderText({input$numSample})
    output$sampleSize <- renderText({input$sampleSize})
    output$popVar <- renderText({popVar()})
    output$biaVar <- renderText({
        sample<- as.data.frame(matrix(bootstrapSample(), nrow = input$numSample,ncol =input$sampleSize))
        return(round(mean(rowSums((sample-rowMeans(sample))^2)/input$sampleSize), 2))
    })
    output$unbiaVar <- renderText({
        sample<- as.data.frame(matrix(bootstrapSample(), nrow = input$numSample,ncol =input$sampleSize))
        return(round(mean(rowSums((sample-rowMeans(sample))^2)/(input$sampleSize-1)),2))
    })
    output$popHist <- renderGvis({
        popHist <- gvisHistogram(data.frame(pop()), options = list(
            height = "300px",
            legend = "{position: 'none'}", title = "Population Distribution",
            subtitle = "samples randomly drawn (with replacement) from values 1 to 20",
            histogram = "{ hideBucketItems: true, bucketSize: 2 }",
            hAxis = "{ title: 'Values', maxAlternation: 1, showTextEvery: 1}",
            vAxis = "{ title: 'Frequency'}"
        ))
        return(popHist)
    })

    output$sampBiaVarHist <- renderGvis({
        sample<- as.data.frame(matrix(bootstrapSample(), nrow = input$numSample,ncol =input$sampleSize))
        estVar <- data.frame(estVar = rowSums((sample-rowMeans(sample))^2)/length(sample))
        popHist <- gvisHistogram(estVar, options = list(
            height = "200px",
            legend = "{position: 'none'}",
            title = paste("Distribution of Biased Sample Variances, mean =", toString(round(mean(estVar$estVar),2))),
            subtitle = "samples randomly drawn (with replacement) from values 1 to 20",
            histogram = "{ hideBucketItems: true, bucketSize: 2 }",
            hAxis = "{ title: 'Values', maxAlternation: 1, showTextEvery: 3, minValue: 0, maxValue: 80}",
            vAxis = "{ title: 'Frequency'}"
        ))
        return(popHist)
    })
    output$sampUnbiaVarHist <- renderGvis({
        sample<- as.data.frame(matrix(bootstrapSample(), nrow = input$numSample,ncol =input$sampleSize))
        estVar <- data.frame(estVar = rowSums((sample-rowMeans(sample))^2)/(length(sample)-1))
        popHist <- gvisHistogram(estVar, options = list(
            height = "200px",
            legend = "{position: 'none'}",
            title = paste("Distribution of Unbiased Sample Variances, mean =", toString(round(mean(estVar$estVar),2))),
            subtitle = "samples randomly drawn (with replacement) from values 1 to 20",
            histogram = "{ hideBucketItems: true, bucketSize: 2 }",
            hAxis = "{ title: 'Values', maxAlternation: 1, showTextEvery: 3, minValue: 0, maxValue: 80}",
            vAxis = "{ title: 'Frequency'}"
        ))
        return(popHist)
    })
    output$varPlot <- renderPlot({
        sample<- as.data.frame(matrix(bootstrapSample(), nrow = input$numSample,ncol =input$sampleSize))
        estVar <- data.frame(estVar = rowSums(
            (sample-rowMeans(sample))^2)/input$sampleSize)
        difference <- estVar - var(pop())
        difference <- cbind(index = 1:nrow(difference), difference)
        varPlot <- ggplot(data = difference, aes(x = index, y = estVar)) +
            geom_point() +
            geom_hline(yintercept=0, col = "red", size = 2) + geom_smooth() +
            ggtitle("Difference Between Population and Biased Sample Variances") +
            labs(x = "Sample", y = "Biased Variance - Population Variance")
        varPlot
    })
    output$unbiaVarPlot <- renderPlot({
        sample<- as.data.frame(matrix(bootstrapSample(), nrow = input$numSample,ncol =input$sampleSize))
        estVar <- data.frame(estVar = rowSums((sample-rowMeans(sample))^2)/(input$sampleSize-1))
        difference <- estVar - var(pop())
        difference <- cbind(index = 1:nrow(difference), difference)
        varPlot <- ggplot(data = difference, aes(x = index, y = estVar)) +
            geom_point() +
            geom_hline(yintercept=0, col = "red", size = 2) + geom_smooth() +
            ggtitle("Difference Between Population and Unbiased Sample Variances") +
            labs(x = "Sample", y = "Unbiased Variance - Population Variance")
        varPlot
    })
    output$varDiffPlot <- renderGvis({
        sample<- as.data.frame(matrix(bootstrapSample(), nrow = input$numSample,ncol =input$sampleSize))
        result <- data.frame(size = numeric(0), meanVar = numeric(0))
        suppressWarnings(data <- melt(sample)$value)
        for(n in 2:input$sampleSize){
            row <- floor(length(data)/n)
            tempData <- data[1:(row*n)]
            dim(tempData) <- c(row, n)
            result[n-1, ] <- c(n, round(mean(
                rowSums((tempData-rowMeans(tempData))^2)/n)/popVar(), 2))
        }
        hAxisLabel <- paste(2:input$sampleSize, collapse = ",")
        hAxisOptionsStr <- paste(
            "{ title: 'Sample Size', gridlines : {count: -1}, minValue: 2, ticks: [",
            hAxisLabel, "]}", sep = "")
        varDiffPlot <- gvisColumnChart(result, xvar = "size", yvar = "meanVar",
                                       options = list(
                                           height = "200px",
                                           legend = "{position: 'none'}",
                                           title = "Mean Biased Sample Variance vs Sample Size",
                                           hAxis = hAxisOptionsStr,
                                           vAxis = "{ title: 'Percent of Population Variance \\ (%)',
                                        minValue: 0, format: '##%' }"
                                       ))
        return(varDiffPlot)
    })
    output$unbiaVarDiffPlot <- renderGvis({
        sample<- as.data.frame(matrix(bootstrapSample(), nrow = input$numSample,ncol =input$sampleSize))
        result <- data.frame(size = numeric(0), meanVar = numeric(0))
        suppressWarnings(data <- melt(sample)$value)
        for(n in 2:input$sampleSize){
            row <- floor(length(data)/n)
            tempData <- data[1:(row*n)]
            dim(tempData) <- c(row, n)
            result[n-1, ] <- c(n, round(mean(
                rowSums((tempData-rowMeans(tempData))^2)/(n-1))/popVar(), 2))
        }
        hAxisLabel <- paste(2:input$sampleSize, collapse = ",")
        hAxisOptionsStr <- paste(
            "{ title: 'Sample Size', gridlines : {count: -1}, minValue: 2, ticks: [",
            hAxisLabel, "]}", sep = "")
        varDiffPlot <- gvisColumnChart(result, xvar = "size", yvar = "meanVar",
                                       options = list(
                                           height = "200px",
                                           legend = "{position: 'none'}",
                                           title = "Mean Unbiased Sample Variance vs Sample Size",
                                           hAxis = hAxisOptionsStr,
                                           vAxis = "{ title: 'Percent of Population Variance \\ (%)',
                                        minValue: 0, format: '##%' }"
                                       ))
        return(varDiffPlot)
    })
})
