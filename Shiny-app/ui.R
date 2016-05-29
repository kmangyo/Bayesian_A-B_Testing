library(shiny)
library(broom)
library(dplyr)
library(ggplot2)
theme_set(theme_bw())

shinyUI(fluidPage(
	titlePanel("Bayesian A/B Testing"),
	mainPanel(
		h6("- beta1, beta2는 사전 확률을 의미합니다."),
		h6("- 예를들어 CVR이 보통 10%면, beta1은 1 beta2는 9를 입력하면 됩니다. 1%면 beta1은 0.1 beta2는 0.9를 입력하면 됩니다."),
		column(2, textInput("beta1", label = h6("beta1"), value = ""),
		textInput("beta2", label = h6("beta2"), value = "")),

		column(2, textInput("clicks_A", label = h6("clicks_A"), value = ""),
		textInput("clicks_B", label = h6("clicks_B"), value = "")),

		column(2, textInput("installs_A", label = h6("installs_A"), value = ""),
		textInput("installs_B", label = h6("installs_B"), value = ""),submitButton("Update View")),

		column(10, h4("Result"),
		h6("- Result는 A보다 B의 CVR이 높을 확률을 의미합니다."),
     	h6("- A는 A의 CVR을 의미합니다."),
     	h6("- B는 B의 CVR을 의미합니다."),
     	verbatimTextOutput("test.result"), 

     	h4(""),
     	h4("Result Density"),
     	plotOutput("graph_plot"), 

     	h4(""),
     	h4("Impact"), 
     	h6("- 효과의 크기를 의미합니다."),
     	h6("- 예를들어 10%의 해당값이 1.20이면 A가 B보다 120% 향상될 확률이 90%라는 의미입니다."),
     	verbatimTextOutput("quan"))
    )
))
