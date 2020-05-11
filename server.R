library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)

kickstarter <- read.csv(".\\data\\kickstarter_df2.csv")
kick_art <- read.csv(".\\data\\kickstarter_art.csv")
kick_comics <- read.csv(".\\data\\kickstarter_comics.csv")
kick_crafts <- read.csv(".\\data\\kickstarter_crafts.csv")
kick_dance <- read.csv(".\\data\\kickstarter_dance.csv")
kick_design <- read.csv(".\\data\\kickstarter_design.csv")
kick_fashion <- read.csv(".\\data\\kickstarter_fashion.csv")
kick_film <- read.csv(".\\data\\kickstarter_film.csv")
kick_food <- read.csv(".\\data\\kickstarter_food.csv")
kick_games <- read.csv(".\\data\\kickstarter_games.csv")
kick_journalism <- read.csv(".\\data\\kickstarter_journalism.csv")
kick_music <- read.csv(".\\data\\kickstarter_music.csv")
kick_photography <- read.csv(".\\data\\kickstarter_photography.csv")
kick_publishing <- read.csv(".\\data\\kickstarter_publishing.csv")
kick_technology <- read.csv(".\\data\\kickstarter_technology.csv")
kick_theater <- read.csv(".\\data\\kickstarter_theater.csv")

shinyServer(function(input,output){
  output$art <- renderPlot({
    kick_art %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged")+
      scale_x_continuous(labels = kick_art$year, breaks=kick_art$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$art_camp <- renderPlot({
    kick_art %>%
      ggplot(aes(x=duration, y=pledged_usd, label=name))+
      geom_point(aes(colour = status), size = 4)+
      ylim(input$art_lim[1],input$art_lim[2]) +
      geom_text(aes(label=ifelse(pledged_usd>(input$art_lim[2]/2),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })
  
  output$art_stat <- renderPlot({
    kick_art %>%
      ggplot(aes(x=reorder(sub_category,status == input$art_stat_1,sum),fill = status))+
      geom_bar(position="stack")+ 
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of art")+
      ylab("number of projects by status")
  })
  
  output$comics <- renderPlot({
    kick_comics %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged per year")+
      scale_x_continuous(labels = kick_comics$year, breaks=kick_comics$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$comics_camp <- renderPlot({
    kick_comics %>%
      ggplot(aes(x=duration, y=pledged_usd))+
      geom_point(aes(colour = factor(status)), size = 4)+
      ylim(input$comics_lim[1],input$comics_lim[2]) +
      geom_text(aes(label=ifelse(pledged_usd>(input$comics_lim[2]/2),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })
  
  output$comics_stat <- renderPlot({
    kick_comics %>%
      ggplot(aes(x=reorder(sub_category,status == input$comics_stat_1,sum),fill = status))+
      geom_bar(position="stack")+ 
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of comics")+
      ylab("number of projects by status")
  })
  
  output$crafts <- renderPlot({
    kick_crafts %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged")+
      scale_x_continuous(labels = kick_crafts$year, breaks=kick_crafts$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$crafts_camp <- renderPlot({
    kick_crafts %>%
      ggplot(aes(x=duration, y=pledged_usd))+
      geom_point(aes(colour = factor(status)), size = 4)+
      ylim(input$crafts_lim[1],input$crafts_lim[2])+
      geom_text(aes(label=ifelse(pledged_usd>(input$crafts_lim[2]/2),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })

  output$crafts_stat <- renderPlot({
    kick_crafts %>%
      ggplot(aes(x=reorder(sub_category,status == input$crafts_stat_1,sum),fill = status))+
      geom_bar(position="stack")+ 
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of crafts")+
      ylab("number of projects by status")
  })
  
  output$dance <- renderPlot({
    kick_dance %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged")+
      scale_x_continuous(labels = kick_dance$year, breaks=kick_dance$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$dance_camp <- renderPlot({
    kick_dance %>%
      ggplot(aes(x=duration, y=pledged_usd))+
      geom_point(aes(colour = factor(status)), size = 4)+
      ylim(input$dance_lim[1],input$dance_lim[2])+
      geom_text(aes(label=ifelse(pledged_usd>(input$dance_lim[2]/2),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })
  
  output$dance_stat <- renderPlot({
    kick_dance %>%
      ggplot(aes(x=reorder(sub_category,status == input$dance_stat_1,sum),fill = status))+
      geom_bar(position="stack")+ 
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of dance")+
      ylab("number of projects by status")
  })
  
  output$design <- renderPlot({
    kick_design %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged")+
      scale_x_continuous(labels = kick_design$year, breaks=kick_design$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$design_camp <- renderPlot({
    kick_art %>%
      ggplot(aes(x=duration, y=pledged_usd))+
      geom_point(aes(colour = factor(status)), size = 4)+
      ylim(input$design_lim[1],input$design_lim[2])+
      geom_text(aes(label=ifelse(pledged_usd>(input$design_lim[2]/2),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })
  
  output$design_stat <- renderPlot({
    kick_design %>%
      ggplot(aes(x=reorder(sub_category,status == input$design_stat_1,sum),fill = status))+
      geom_bar(position="stack")+ 
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of design")+
      ylab("number of projects by status")
  })
  
  output$fashion <- renderPlot({
    kick_fashion %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged")+
      scale_x_continuous(labels = kick_fashion$year, breaks=kick_fashion$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$fashion_camp <- renderPlot({
    kick_fashion %>%
      ggplot(aes(x=duration, y=pledged_usd))+
      geom_point(aes(colour = factor(status)), size = 4)+
      ylim(input$fashion_lim[1],input$fashion_lim[2])+
      geom_text(aes(label=ifelse(pledged_usd>(input$fashion_lim[2]/2),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })
  
  output$fashion_stat <- renderPlot({
    kick_fashion %>%
      ggplot(aes(x=reorder(sub_category,status == input$fashion_stat_1,sum),fill = status))+
      geom_bar(position="stack")+ 
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of fashion")+
      ylab("number of projects by status")
  })
  
  output$film <- renderPlot({
    kick_film %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged")+
      scale_x_continuous(labels = kick_film$year, breaks=kick_film$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$film_camp <- renderPlot({
    kick_film %>%
      ggplot(aes(x=duration, y=pledged_usd))+
      geom_point(aes(colour = factor(status)), size = 4)+
      ylim(input$film_lim[1],input$film_lim[2])+
      geom_text(aes(label=ifelse(pledged_usd>(input$film_lim[2]/2),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })
  
  output$film_stat <- renderPlot({
    kick_film %>%
      ggplot(aes(x=reorder(sub_category,status == input$film_stat_1,sum),fill = status))+
      geom_bar(position="stack")+ 
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of film")+
      ylab("number of projects by status")
  })
  
  output$food <- renderPlot({
    kick_food %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged")+
      scale_x_continuous(labels = kick_food$year, breaks=kick_food$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$food_camp <- renderPlot({
    kick_food %>%
      ggplot(aes(x=duration, y=pledged_usd))+
      geom_point(aes(colour = factor(status)), size = 4)+
      ylim(input$food_lim[1],input$food_lim[2])+
      geom_text(aes(label=ifelse(pledged_usd>(input$food_lim[2]/2),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })
  
  output$food_stat <- renderPlot({
    kick_food %>%
      ggplot(aes(x=reorder(sub_category,status == input$food_stat_1,sum),fill = status))+
      geom_bar(position="stack")+ 
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of food")+
      ylab("number of projects by status")
  })
  
  output$games <- renderPlot({
    kick_games %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged")+
      scale_x_continuous(labels = kick_games$year, breaks=kick_games$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$games_camp <- renderPlot({
    kick_games %>%
      ggplot(aes(x=duration, y=pledged_usd))+
      geom_point(aes(colour = factor(status)), size = 4)+
      ylim(input$games_lim[1],input$games_lim[2])+
      geom_text(aes(label=ifelse(pledged_usd>(input$games_lim[2]/2),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })
  
  output$games_stat <- renderPlot({
    kick_games %>%
      ggplot(aes(x=reorder(sub_category,status == input$games_stat_1,sum),fill = status))+
      geom_bar(position="stack")+ 
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of games")+
      ylab("number of projects by status")
  })
  
  output$journalism <- renderPlot({
    kick_journalism %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged")+
      scale_x_continuous(labels = kick_journalism$year, breaks=kick_journalism$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$journalism_camp <- renderPlot({
    kick_journalism %>%
      ggplot(aes(x=duration, y=pledged_usd))+
      geom_point(aes(colour = factor(status)), size = 4)+
      ylim(input$journalism_lim[1],input$journalism_lim[2])+
      geom_text(aes(label=ifelse(pledged_usd>(input$journalism_lim[2]/2),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })
  
  output$journalism_stat <- renderPlot({
    kick_journalism %>%
      ggplot(aes(x=reorder(sub_category,status == input$journalism_stat_1,sum),fill = status))+
      geom_bar(position="stack")+ 
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of journalism")+
      ylab("number of projects by status")
  })
  
  output$music <- renderPlot({
    kick_music %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged")+
      scale_x_continuous(labels = kick_music$year, breaks=kick_music$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$music_camp <- renderPlot({
    kick_music %>%
      ggplot(aes(x=duration, y=pledged_usd))+
      geom_point(aes(colour = factor(status)), size = 4)+
      ylim(input$music_lim[1],input$music_lim[2])+
      geom_text(aes(label=ifelse(pledged_usd>(input$music_lim[2]/2),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })
  
  output$music_stat <- renderPlot({
    kick_music %>%
      ggplot(aes(x=reorder(sub_category,status == input$music_stat_1,sum),fill = status))+
      geom_bar(position="stack")+ 
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of music")+
      ylab("number of projects by status")
  })
  
  output$photography <- renderPlot({
    kick_photography %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged")+
      scale_x_continuous(labels = kick_photography$year, breaks=kick_photography$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$photography_camp <- renderPlot({
    kick_photography %>%
      ggplot(aes(x=duration, y=pledged_usd))+
      geom_point(aes(colour = factor(status)), size = 4)+
      ylim(input$photography_lim[1],input$photography_lim[2])+
      geom_text(aes(label=ifelse(pledged_usd>(input$photography_lim[2]/2),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })
  
  output$photography_stat <- renderPlot({
    kick_photography %>%
      ggplot(aes(x=reorder(sub_category,status == input$photography_stat_1,sum),fill = status))+
      geom_bar(position="stack")+ 
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of photography")+
      ylab("number of projects by status")
  })
  
  output$publishing <- renderPlot({
    kick_publishing %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged")+
      scale_x_continuous(labels = kick_publishing$year, breaks=kick_publishing$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$publishing_camp <- renderPlot({
    kick_publishing %>%
      ggplot(aes(x=duration, y=pledged_usd))+
      geom_point(aes(colour = factor(status)), size = 4)+
      ylim(input$publishing_lim[1],input$publishing_lim[2])+
      geom_text(aes(label=ifelse(pledged_usd>(input$publishin_lim[2]/2),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })
  
  output$publishing_stat <- renderPlot({
    kick_publishing %>%
      ggplot(aes(x=reorder(sub_category,status == input$publishing_stat_1,sum),fill = status))+
      geom_bar(position="stack")+ 
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of publishing")+
      ylab("number of projects by status")
  })
  
  output$technology <- renderPlot({
    kick_technology %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged")+
      scale_x_continuous(labels = kick_technology$year, breaks=kick_technology$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$technology_camp <- renderPlot({
    kick_technology %>%
      ggplot(aes(x=duration, y=pledged_usd))+
      geom_point(aes(colour = factor(status)), size = 4)+
      ylim(input$technology_lim[1],input$technology_lim[2])+
      geom_text(aes(label=ifelse(pledged_usd>(input$technology_lim[2]/2),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })
  
  output$technology_stat <- renderPlot({
    kick_technology %>%
      ggplot(aes(x=reorder(sub_category,status == input$technology_stat_1,sum),fill = status))+
      geom_bar(position="stack") +
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of technology")+
      ylab("number of projects by status")
  })
  
  output$theater <- renderPlot({
    kick_theater %>%
      ggplot(aes(x=year, y=pledged_usd))+
      geom_bar(fill = "#05ce78", 
               stat = 'identity')+
      ggtitle("Amount Pledged")+
      scale_x_continuous(labels = kick_theater$year, breaks=kick_theater$year)+
      xlab("year")+
      ylab("amount pledged (USD)")
  })
  
  output$theater_camp <- renderPlot({
    kick_theater %>%
      ggplot(aes(x=duration, y=pledged_usd))+
      geom_point(aes(colour = status), size = 4)+
      ylim(input$theater_lim[1],input$theater_lim[2])+
      geom_text(aes(label=ifelse(pledged_usd>(input$theater_lim[2]/4),as.character(name),'')),hjust=0,vjust=0)+
      scale_color_manual(values = c("#424949", "#BDC3C7", "#05CE78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      xlab("length of campaign(days)")+
      ylab("amount pledged (USD)")
  })
  
  output$theater_stat <- renderPlot({
    kick_theater %>%
      ggplot(aes(x=reorder(sub_category,status == input$theater_stat_1,sum),fill = status))+
      geom_bar(position="stack")+
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())+
      coord_flip ()+
      xlab("subcategories of theater")+
      ylab("number of projects by status")
  })
  
  output$len_camp <- renderPlot({
    kickstarter %>%
      ggplot(aes(x=quarter,fill = status))+
      geom_bar(position="dodge")+ 
      geom_text(aes(label=stat(count)),
                stat='count', 
                vjust=-0.25,
                position = position_dodge(width = 1))+
      scale_fill_manual(values = c("#424949", "#BDC3C7", "#05ce78"))+
      theme(legend.justification=c(0,1), 
            legend.position=c(0.85, 0.85),
            legend.background = element_blank(),
            legend.key = element_blank())
  })
})

