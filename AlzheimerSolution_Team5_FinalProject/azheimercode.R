
install.packages("RMySQL")

library(RMySQL)

#Connecting to database
mydb = dbConnect(MySQL(), user='root', password='root', dbname='alzheimer_db', host='localhost')

#Selecting the columns and aggregating the scores (total score/day difference) for each of the tests and joining data for all the tests
rs = dbSendQuery(mydb, "select m.RID, m.comp_MMSE_score, n.comp_FAQ_total,o.comp_NPI_total,
                  p.comp_GDS_total, s.comp_CDR_total,t.comp_MOCA_total from
                 
                 (select a.RID,MMSCORE/daydiff as comp_MMSE_score from 
                 (select RID,sum(MMSCORE) as MMSCORE from MMSE_clean group by RID) a
                 inner join
                 (select RID,datediff(max(update_stamp),min(update_stamp)) as daydiff
                 from MMSE_clean group by RID) b on a.RID=b.RID where 
                 (MMSCORE/daydiff) is not null) m 
                 
                 left join
                 
                 (select c.RID,FAQTOTAL/daydiff as comp_FAQ_total from 
                 (select RID,sum(FAQTOTAL) as FAQTOTAL from FAQ_clean group by RID) c
                 inner join
                 (select RID,datediff(max(update_stamp),min(update_stamp)) as daydiff
                 from FAQ_clean group by RID) d on c.RID=d.RID where 
                 (FAQTOTAL/daydiff) is not null) n on m.RID=n.RID
                 
                 left join 
                 
                 (select e.RID,NPITOTAL/daydiff as comp_NPI_total from 
                 (select RID,sum(NPITOTAL) as NPITOTAL from NPI_clean group by RID) e
                 inner join
                 (select RID,datediff(max(update_stamp),min(update_stamp)) as daydiff
                 from NPI_clean group by RID) f on e.RID=f.RID where 
                 (NPITOTAL/daydiff) is not null) o on o.RID=n.RID
                 
                 left join
                 
                 (select g.RID,GDTOTAL/daydiff as comp_GDS_total from 
                 (select RID,sum(GDTOTAL) as GDTOTAL from GDS_clean group by RID) g
                 inner join
                 (select RID,datediff(max(update_stamp),min(update_stamp)) as daydiff
                 from GDS_clean group by RID) h on g.RID=h.RID where 
                 (GDTOTAL/daydiff) is not null) p on p.RID=o.RID
                 
                 left join
                 
                 (select v.RID,CDGLOBAL/daydiff as comp_CDR_total from 
                 (select RID,sum(CDGLOBAL) as CDGLOBAL from CDR_clean group by RID) v
                 inner join
                 (select RID,datediff(max(update_stamp),min(update_stamp)) as daydiff
                 from CDR_clean group by RID) w on v.RID=w.RID where 
                 (CDGLOBAL/daydiff) is not null) s on s.RID = p.RID
                 
                 left join
                 
                 (select k.RID,MOCA_TOTAL/daydiff as comp_MOCA_total from 
                 (select RID,sum(MOCA_TOTAL) as MOCA_TOTAL from MOCA_clean group by RID) k
                 inner join
                 (select RID,datediff(max(update_stamp),min(update_stamp)) as daydiff
                 from MOCA_clean group by RID) l on k.RID=l.RID where 
                 (MOCA_TOTAL/daydiff) is not null) t on t.RID=s.RID")



data = fetch(rs, n=-1)

psychpca = data[,2:6]
#Omitting NA and performing principal component analysis
pca_comp <- prcomp(na.omit(psychpca), scale. = TRUE)
