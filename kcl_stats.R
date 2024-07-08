bluecaps <- read.csv('Blue Caps 2024 Summer 2024 Stats.csv', header=T)
names(bluecaps) <- bluecaps[1,]
bluecaps <- bluecaps[-1,]
num_del <- nrow(bluecaps) - 2
bluecaps <- bluecaps[-c(num_del:nrow(bluecaps)),]

bobcats <- read.csv('Bobcats 2024 Summer 2024 Stats.csv', header=T)
names(bobcats) <- bobcats[1,]
bobcats <- bobcats[-1,]
num_del <- nrow(bobcats) - 2
bobcats <- bobcats[-c(num_del:nrow(bobcats)),]

merchants <- read.csv('Merchants 2024 Summer 2024 Stats.csv', header=T)
names(merchants) <- merchants[1,]
merchants <- merchants[-1,]
num_del <- nrow(merchants) - 2
merchants <- merchants[-c(num_del:nrow(merchants)),]

groundsloths <- read.csv('Ground Sloths 2024 Summer 2024 Stats.csv', header=T)
names(groundsloths) <- groundsloths[1,]
groundsloths <- groundsloths[-1,]
num_del <- nrow(groundsloths) - 2
groundsloths <- groundsloths[-c(num_del:nrow(groundsloths)),]

bluecaps_batting <- bluecaps[c(1:54)]
bluecaps_batting <- bluecaps_batting[which(bluecaps_batting$PA>0),]
write.csv(bluecaps_batting, '~/Downloads/CornBelters/blc_hit.csv', row.names=F)
bluecaps_pitching <- bluecaps[c(1:3, 55:112)]
bluecaps_pitching <- bluecaps_pitching[which(as.numeric(bluecaps_pitching$IP)>0),]
write.csv(bluecaps_pitching, '~/Downloads/CornBelters/blc_pitch.csv', row.names=F)
bluecaps_fielding <- bluecaps[c(1:4, 143:157)]
write.csv(bluecaps_fielding, '~/Downloads/CornBelters/blc_field.csv', row.names=F)

bobcats_batting <- bobcats[c(1:54)]
bobcats_batting <- bobcats_batting[which(bobcats_batting$PA>0),]
write.csv(bobcats_batting, '~/Downloads/CornBelters/boc_hit.csv', row.names=F)
bobcats_pitching <- bobcats[c(1:3, 55:112)]
bobcats_pitching <- bobcats_pitching[which(as.numeric(bobcats_pitching$IP)>0),]
write.csv(bobcats_pitching, '~/Downloads/CornBelters/boc_pitch.csv', row.names=F)
bobcats_fielding <- bobcats[c(1:4, 143:157)]
write.csv(bobcats_fielding, '~/Downloads/CornBelters/boc_field.csv', row.names=F)

merchants_batting <- merchants[c(1:54)]
merchants_batting <- merchants_batting[which(merchants_batting$PA>0),]
write.csv(merchants_batting, '~/Downloads/CornBelters/mer_hit.csv', row.names=F)
merchants_pitching <- merchants[c(1:3, 55:112)]
merchants_pitching <- merchants_pitching[which(as.numeric(merchants_pitching$IP)>0),]
write.csv(merchants_pitching, '~/Downloads/CornBelters/mer_pitch.csv', row.names=F)
merchants_fielding <- merchants[c(1:4, 143:157)]
write.csv(merchants_fielding, '~/Downloads/CornBelters/mer_field.csv', row.names=F)

groundsloths_batting <- groundsloths[c(1:54)]
groundsloths_batting <- groundsloths_batting[which(groundsloths_batting$PA>0),]
write.csv(groundsloths_batting, '~/Downloads/CornBelters/grs_hit.csv', row.names=F)
groundsloths_pitching <- groundsloths[c(1:3, 55:112)]
groundsloths_pitching <- groundsloths_pitching[which(as.numeric(groundsloths_pitching$IP)>0),]
write.csv(groundsloths_pitching, '~/Downloads/CornBelters/grs_pitch.csv', row.names=F)
groundsloths_fielding <- groundsloths[c(1:4, 143:157)]
write.csv(groundsloths_fielding, '~/Downloads/CornBelters/grs_field.csv', row.names=F)

