##word_dims is an array with words in row 1, pixel lengths in row 2, heights
##(if needed) in row 3

line_number<-function(word_dims,space_length,margin){
	lines=1
	tot_length=0
	word_lines<-array(dim=c(dim(word_dims)[[1]],4))
	word_lines[,1:3]<-word_dims
	for(i in 1:dim(word_dims)[[1]]){
##"\r" denotes a return in the text; two denote a blank line
		if(word_dims[i,1]=="\r"){
			tot_length=0
			word_lines[i,4]<-lines
			lines=lines+1
		}
		else{
			if(tot_length+as.numeric(word_dims[i,2])+space_length>(2*margin)){
				tot_length=as.numeric(word_dims[i,2])
				lines=lines+1
				word_lines[i,4]<-lines
			}
			else{
				tot_length<-tot_length+as.numeric(word_dims[i,2])+space_length
				word_lines[i,4]<-lines
			}
		}
	}
	print(word_lines)
}

## to work on for word_pos function
## what if \r begins the chunk?
## try modularizing the x-coordinate position
## how can I generalize this more for future studies

word_pos<-function(word_dims,line_height,space_length,margin){
## look more closely at xcoord designations here - do they work for any possible inputs?
	word_lines<-line_number(word_dims,space_length)
	## calls line_number to make a new array where each word is assigned a line number
	ycoord1=as.numeric(word_lines[,4])/2*line_height
	ycoord2=(as.numeric(word_lines[,4])/2-1)*line_height
	lines=0
	## word_positions columns represent the word, left x-coord, right x-coord, top y-coord, and bottom y-coord
	word_positions<-array(dim=c(dim(word_lines)[[1]],5))
	word_positions[,1]<-word_lines[,1]
	## following two lines calculate all y-positions at once based on word_lines under vertical centering assumption
	word_positions[,4]<-(max(as.numeric(word_lines[,4]))/2-as.numeric(word_lines[,4])+1)*line_height
	word_positions[,5]<-(max(as.numeric(word_lines[,4]))/2-as.numeric(word_lines[,4]))*line_height
	xcoord1=-margin
	xcoord2=-margin-space_length
	for(i in 1:dim(word_lines)[[1]]){
		if(word_lines[i,1]=="\r"){
			xcoord2->word_positions[i,2]
			margin->word_positions[i,3]
			## this could be a problem - we want the box to encompass the rest of the line if one \r, the whole line if two
			xcoord1=-margin
			xcoord2=-margin-space_length
		}
		else{
			if(xcoord2+as.numeric(word_lines[i,2])+space_length<margin){
				xcoord1<-xcoord2+space_length
				xcoord2<-xcoord1+as.numeric(word_lines[i,2])
				xcoord1->word_positions[i,2]
				xcoord2->word_positions[i,3]
			}
			else{
				xcoord1=-margin
				xcoord2=xcoord1+as.numeric(word_lines[i,2])
				xcoord1->word_positions[i,2]
				xcoord2->word_positions[i,3]
			}
		}
	}
print(word_positions)
}