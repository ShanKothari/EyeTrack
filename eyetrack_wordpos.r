#####################################
## Note: this code is for determining the screen position of words from a text
## given assumptions of vertical centering, and strict margins
## It was intended for use with the Digital Humanities and Literary Cognition Lab's
## Mansfield Park fMRI project to replicate PsychoPy's text placement.
##
## It calls the function line_number to determine the number of lines in the text
## for use in vertical centering
#####################################

##word_dims is an array with words in row 1, pixel lengths of words in row 2, heights
##(if needed) in row 3

## to work on for word_pos function
## what if \r begins the chunk?
## try modularizing the x-coordinate position
## how can I generalize this more for future studies

word_pos<-function(word_dims,line_height,space_length,margin){
## look more closely at xcoord designations here - do they work for any possible inputs?
	word_lines<-line_number(word_dims,space_length)
	## calls line_number to make a new array (word_lines) where each word is assigned a line number
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
