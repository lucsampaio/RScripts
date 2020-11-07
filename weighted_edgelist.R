'''
Script written as support in obtaining a weighted undirected edgelist from  data about the nodes of a network. Used by author Luciano de Sampaio Soares <luciano.soares(a)unir.br/https://www.lucsampaio.me> for his masters dissertation on Facebook image sharing that can be found (in Portuguese) at the link above.
'''
edgeCalc <- function(){
    input <- read.csv("NODE_FILENAME.CSV",header=TRUE,stringsAsFactors=FALSE,sep=";") # Input the node table file (in .csv format) within the quotes
    output <- read.csv("arestas_R.csv", header=TRUE,stringsAsFactor=FALSE,skip=1, # These lines load a blank .csv file with columns == col.names in line 7)
                        colClasses=c("integer","integer","integer"),
                        col.names=c("Source","Target","Weight"))    
    for(i in 1:nrow(input)){ #image to be compared
        for(j in 1:nrow(input)){ #images that will compare to image
            weight <- 0
            if( i >= j ){
                #skips whenever the script tries to run comparison against images already compared, or against itself
            } else {
                for(k in 1:ncol(input)){ #column by column comparison
                    col <- k
                    if(input[i,k] == input[j,k]){ #edge weight modifier
                        weight <- weight+1
                        }
                }
                print(c("source= ",i,"target= ",j,"weight= ",weight)) #visual feedback of running script
                newRow <- data.frame(Source=i,Target=j,Weight=weight) #create row for compared pair
                output <- rbind(output,newRow) # add edge row to data frame
            }
        }
    }
    write.csv(output,"DESTINATION_FILE.csv") #write data frame to csv file. Input file name within quotes
}


edgeCalc() #please note that depending on node file size, this script may run for a VERY long time.
}


