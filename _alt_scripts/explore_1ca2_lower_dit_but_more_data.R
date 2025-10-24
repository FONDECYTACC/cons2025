kept_1ca2 |> 
  #get cases w/more than one dit,  but not every row is missing
  tidytable::group_by(hash_key, adm_date_num)|>
  tidytable::mutate(count_miss_dit2 = sum(is.na(dit), na.rm=T),
                    ndis_dit = tidytable::n_distinct(dit),
                    ntot_hash_adm2 = n(),
                    rank_by_dit = min_rank(-dit),
                    rank_by_missing = min_rank(n_col_miss),
                    rank_by_empty = min_rank(n_col_empty),
                    ndis_miss_data = tidytable::n_distinct(n_col_miss),
                    ndis_empty_data = tidytable::n_distinct(n_col_empty)
  ) |> 
  tidytable::ungroup() |>  
  dplyr::filter(ndis_dit>1 & 
      count_miss_dit2<ntot_hash_adm2 & 
      ndis_miss_data>1 &
        rank_by_dit==1 &
        rank_by_missing==1) |> 
  View()

SISTRAT23_c1_2010_2022_df_prev1c |> 
  dplyr::filter(hash_key=="0061a8a0213c42d2406464ab240565f4e0309083878eb8e699105dbc84036876") |> glimpse()