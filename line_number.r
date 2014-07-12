######################################
## This line_number function determines the number of lines
## in the text for use in vertical centering in eyetrack_wordpos.r
######################################

##word_dims is an array with words in text in row 1, pixel lengths in row 2, heights
##(if needed) in row 3

line_number<-function(word_dims,space_length,margin){
	lines=1
	tot_length=0
	word_lines<-array(dim=c(dim(word_dims)[[1]],4))
	word_lines[,1:3]<-word_dims
	## column 4 of word_lines encodes the line number that will be assigned to each word
	for(i in 1:dim(word_dims)[[1]]){
		##"\r" denotes a return in the text; two denote a blank line
		if(word_dims[i,1]=="\r"){
			tot_length=0
			word_lines[i,4]<-lines
			lines=lines+1
		}
		## starts a new line when margin is reached
		else{
			if(tot_length+as.numeric(word_dims[i,2])+space_length>(2*margin)){
			## assigns length of new line as length of first word on that line	
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