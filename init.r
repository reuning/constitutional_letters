library(rvest)
library(stringr)
library(tidyverse)


main_page <- "https://memory.loc.gov/ammem/amlaw/lwdglink.html"
base_url <- "https://memory.loc.gov"
urls <- read_html(main_page) |> html_element("table.shade") |>
    html_elements("a") |> 
    html_attr("href")

get_letters <- function(url){
    items <- network <- read_html(url) |> 
        html_elements( "a[href*=cgi-bin]")  

    links <- items |> html_attr("href")
    out <- items |>    
        html_text() |> 
        str_match(  pattern="(.*) to (.*)") |> 
        as_tibble(.name_repair="unique") |> 
        rename("text"=1, "head"=2, "tail"=3) |> 
        mutate(url= links) |> 
        drop_na(text) |>
        filter(!(grepl("Delegates", text) | 
                grepl("Committee", text) | 
                grepl("Congress", text) | 
                grepl("Rights", text) | 
                grepl(" and ", text) | 
                grepl("Petition", text) | 
                grepl("Commissioners", text) | 
                grepl("Draft", text))) |> 
        filter(tail!="") |> 
        mutate(head=str_trim(head)) |> 
        mutate(tail=str_trim(tail)) 
    out
}

library(future)
library(future.apply)

plan(multisession)

all_data <- future_lapply(paste0(base_url, urls), get_letters) |> 
    list_rbind()

get_date <- function(data, base){
    data$date <- NA
    for(ii in seq_len(nrow(data))){
        Sys.sleep(runif(1, 0, .5)) 
        full_url <- paste0(base, data$url[ii])

        data$date[ii] <- read_html(full_url) |> 
            html_element("a:contains('Link to date-related documents.')") |> 
            html_attr("name") 
    }

    data

}
split_data <- split(all_data, seq(1, nrow(all_data), length.out = 16))
full_data <- future_lapply(split_data, get_date, base=base_url) |> list_rbind()


full_data <- full_data |> mutate(date= as.Date(date, format="x%Y%m%d"))
write_csv(full_data, "const_letters.csv")
S