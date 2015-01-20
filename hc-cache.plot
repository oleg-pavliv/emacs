set xdata time
set timefmt "%Y-%m-%d %H:%M:%S"
show timefmt
set format x "%d/%H:%M"
set logscale y
set title $dat-file
plot $dat-file using 1:3 with lines title 'PAssetMetadata', $dat-file using 1:4 with lines title 'PEpgData', $dat-file using 1:5 with lines title 'PVodItem', $dat-file using 1:6 with lines title 'PContent', $dat-file using 1:7 with lines title 'PVodItemNodeLink', $dat-file using 1:8 with lines title 'PAsset', $dat-file using 1:9 with lines title 'PCatalogueNode', $dat-file using 1:10 with lines title 'PProduct'
