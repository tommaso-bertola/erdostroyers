cat CA/nodes.csv | tr -d "\"array(["  | tr -d "])\"" | tr -d " " > CA/nodes_cleaned.csv 
sed "1s/.*/index,xpos,ypos/" CA/nodes_cleaned.csv > CA/nodes_header.csv
rm CA/nodes_cleaned.csv
mv CA/nodes_header.csv CA/nodes_cleaned.csv 

cat network/nodes.csv | tr -d "\"array(["  | tr -d "])\"" | tr -d " " > network/nodes_cleaned.csv 
sed "1s/.*/index,label,xpos,ypos/" network/nodes_cleaned.csv > network/nodes_header.csv
rm network/nodes_cleaned.csv
mv network/nodes_header.csv network/nodes_cleaned.csv 
