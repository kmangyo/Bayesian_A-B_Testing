library(shiny)
library(scales)
library(broom)
library(ggplot2)
library(dplyr)
theme_set(theme_bw())

shinyServer(
	function(input, output) {

# 본 A/B 테스트는 시도 횟수대비 성공 확률(성공/시도)을 비교하기 위함.
# Beta prior를 사용한 Binomial Distribution 모델링.
# 이 경우는 광고 클릭 대비 앱 인스톨(Install CVR) 상황을 가정.
# A의 경우가 B경우 보다 성공확률(CVR)이 높을 확률을 계산.
# 추가적으로 각각의 CVR도 표기.

	test.result <- reactive({
		beta1 <- as.numeric(input$beta1)
		beta2 <- as.numeric(input$beta2)
		clicks_A <- as.numeric(input$clicks_A)
		clicks_B <- as.numeric(input$clicks_B)
		installs_A <- as.numeric(input$installs_A)
		installs_B <- as.numeric(input$installs_B)
		
		if(is.na(beta1)|is.na(beta2)|is.na(clicks_A)|is.na(clicks_B)|is.na(installs_A)|is.na(installs_B)){
			test.result<-NA
			} else {
			ver.A <- rbeta(1000000,beta1+installs_A,beta2+(clicks_A-installs_A))
			ver.B <- rbeta(1000000,beta1+installs_B,beta2+(clicks_B-installs_B))
			test.result <- sum(ver.A > ver.B)/1000000
			test <- list()
			test$Result <- percent(test.result)
			test$A <- percent(installs_A/clicks_A)
			test$B <- percent(installs_B/clicks_B)
			test
			}
  	})

# 베이지안 접근의 장점중 하나는 효과의 유의성 뿐 아니라, 효과의 크기를 확인할 수 있음.
# 두 케이스(A/B)의 Beta distribution에 따른 시뮬레이션 결과값을 비교하여 그 크기를 확인.

	quan <- reactive({
		beta1 <- as.numeric(input$beta1)
		beta2 <- as.numeric(input$beta2)
		clicks_A <- as.numeric(input$clicks_A)
		clicks_B <- as.numeric(input$clicks_B)
		installs_A <- as.numeric(input$installs_A)
		installs_B <- as.numeric(input$installs_B)
		
		if(is.na(beta1)|is.na(beta2)|is.na(clicks_A)|is.na(clicks_B)|is.na(installs_A)|is.na(installs_B)){
			quan <- NA
			} else {
			ver.A <- rbeta(1000000,beta1+installs_A,beta2+(clicks_A-installs_A))
			ver.B <- rbeta(1000000,beta1+installs_B,beta2+(clicks_B-installs_B))
			ecdf <- ver.A/ver.B
			quan <- quantile(ecdf, c(1:10*.1))
			quan
			}
  	})  	
  	
# A/B 케이스 CVR의 Beta distribution에 따른 Density plot를 그리기 위한 데이터 전처리 과정.

	graph <- reactive({
		beta1 <- as.numeric(input$beta1)
		beta2 <- as.numeric(input$beta2)
		clicks_A <- as.numeric(input$clicks_A)
		clicks_B <- as.numeric(input$clicks_B)
		installs_A <- as.numeric(input$installs_A)
		installs_B <- as.numeric(input$installs_B)
		
		if(is.na(beta1)|is.na(beta2)|is.na(clicks_A)|is.na(clicks_B)|is.na(installs_A)|is.na(installs_B)){
			graph <- NA
			} else {
			ver.A.beta1 <- beta1+installs_A
			ver.A.beta2 <- beta2+(clicks_A-installs_A)
			ver.B.beta1 <- beta1+installs_B
			ver.B.beta2 <- beta2+(clicks_B-installs_B)
			graph <- data.frame(id=c('A','B'),beta1=c(ver.A.beta1,ver.B.beta1),beta2=c(ver.A.beta2,ver.B.beta2),CVR=c(installs_A/clicks_A,installs_B/clicks_B))
			graph
			}
  	}) 

	output$test.result <- renderPrint({
	test.result<-test.result()
	test.result
	})

	output$quan <- renderPrint({
	quan <- quan()
	quan <- data.frame(Impact=quan)
	quan
	})

# A/B 케이스 CVR의 Beta distribution에 따른 Density plot.
# 사건 시도 횟수가 적으면 사건 시도 횟수가 많은 경우보다 더 넓은 분포를 보이는 걸 시각적으로 확인할 수 있음. 
# 또한, 두 케이스의 차이가 적으면, 분포가 겹치는 것을 확인할 수 있음. 반면, 두 케이스의 차이가 크면 분포가 떨어져 있는걸 확인할 수 있음.

	output$graph_plot <- renderPlot({
		graph<-graph()
		if(is.na(graph)){
		plot.new()
		} else {
		graph_plot<-graph %>%
  		inflate(x = seq(0, with(graph, max(CVR)*2), with(graph, min(CVR)/100))) %>%
  		mutate(density = dbeta(x, beta1, beta2)) %>%
  		ggplot(aes(x, density, color = id)) +
  		geom_line() +
  		labs(x = "CVR", color = "")
  		print(graph_plot)
  		}
	})
})
