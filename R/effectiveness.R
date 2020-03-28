#' Average Precision at n.
#' n is determined by the boundaries parameter.
#' boundaries parameter means the cutoff points according to the total results.
#'
#' @param interactions A list.
#' @param boundaries A list.
#' @return The results of AP at n calculation based on the boundaries parameter.
#' @export
ap_at_n <- function(interactions, boundaries = list('all'))
{
  ap <- list()
  m_ap <- list()

  for(cutoff in boundaries)
  {
    ap[[as.character(cutoff)]] <- c()
  }

  for (query in interactions)
  {
    total_result <- query$total_result #list: total count of results
    related_documents <- unique(query$related_documents) #the list of documents which is known that related to the query

    #visited_documents: id of each visited page on the result list
    #the reason of using array_unique is to ignore the same page visits (happened repeatedly) in the same query
    visited_documents <- unique(query$visited_documents)

    #visited_documents_orders: the order of each visited page, which it's id is known, on the result list
    visited_documents_orders <- query$visited_documents_orders

    for(cutoff in boundaries)
    {
      cutoff_ <- as.character(cutoff) # it is used as an index value
      if(is.numeric(cutoff))
      {
        if(total_result>=cutoff)
        {
          related <- 0

          for(document_id in visited_documents)
          {
            if(document_id %in% related_documents & visited_documents_orders[[as.character(document_id)]]<=cutoff)
            {
              related <- related + 1
            }
          }

          ap[[cutoff_]] <- append(ap[[cutoff_]], (related/cutoff))
        }
      }
      else
      {
        related <- 0

        for(document_id in visited_documents)
        {
          if(document_id %in% related_documents)
          {
            related <- related + 1
          }
        }

        ap[[cutoff_]] <- append(ap[[cutoff_]], (related/total_result))
      }
    }

  }

  #m_ap calculation over all organised ap values
  for(cutoff in boundaries)
  {
    cutoff_ <- as.character(cutoff)
    m_ap[[cutoff_]] <- list("count" = 0,"value" = 0)

    #is there any value for the related cutoff
    if(length(ap[[cutoff_]])>0)
    {
      m_ap[[cutoff_]]$count <- length(ap[[cutoff_]]) #how many ap was used
      m_ap[[cutoff_]]$value <- sum(ap[[cutoff_]])/length(ap[[cutoff_]])
    }
  }

  return(m_ap)
}

#' Mean Average Precision.
#' boundaries parameter means the cutoff points according to the total results.
#'
#' @param interactions A list.
#' @param boundaries A list.
#' @return The results of MAP calculation based on the boundaries parameter.
#' @export
mean_ap <- function(interactions, boundaries = list('all'))
{
  ap <- list()
  mean_ap <- list()
  total <- list()

  for(cutoff in boundaries)
  {
    ap[[as.character(cutoff)]] <- c()
  }

  for (query in interactions)
  {
    total_result <- query$total_result #list: total count of results
    related_documents <- unique(query$related_documents) #the list of documents which is known that related to the query

    #visited_documents: id of each visited page on the result list
    #the reason of using array_unique is to ignore the same page visits (happened repeatedly) in the same query
    visited_documents <- unique(query$visited_documents)

    #visited_documents_orders: the order of each visited page, which it's id is known, on the result list
    visited_documents_orders <- query$visited_documents_orders[order(unlist(query$visited_documents_orders), decreasing=FALSE)]

    for(cutoff in boundaries)
    {
      cutoff_ <- as.character(cutoff) # it is used as an index value
      pass_ <- visited_documents

      if(is.numeric(cutoff))
      {
        if(total_result>=cutoff)
        {
          index <- as.character(length(ap[[cutoff_]]))
          ap[[cutoff_]][[index]] <- list("value" = 0, "count" = 0)

          for(document in names(visited_documents_orders))
          {
            if(document %in% pass_ & document %in% related_documents & visited_documents_orders[[as.character(document)]]<=cutoff)
            {
              ap[[cutoff_]][[index]]$count <- ap[[cutoff_]][[index]]$count+1
              ap[[cutoff_]][[index]]$value <- ap[[cutoff_]][[index]]$value + ap[[cutoff_]][[index]]$count/visited_documents_orders[[as.character(document)]]

              pass_=setdiff(pass_, c(document))
            }
          }

          ap[[cutoff_]][[index]]$value <- ap[[cutoff_]][[index]]$value/length(related_documents)
        }
      }
      else
      {
        index <- as.character(length(ap[[cutoff_]]))
        ap[[cutoff_]][[index]] <- list("value" = 0, "count" = 0)

        for(document in names(visited_documents_orders))
        {
          if(document %in% pass_ & document %in% related_documents)
          {
            ap[[cutoff_]][[index]]$count <- ap[[cutoff_]][[index]]$count+1
            ap[[cutoff_]][[index]]$value <- ap[[cutoff_]][[index]]$value + ap[[cutoff_]][[index]]$count/visited_documents_orders[[as.character(document)]]

            pass_=setdiff(pass_, c(document))
          }
        }

        ap[[cutoff_]][[index]]$value <- ap[[cutoff_]][[index]]$value/length(related_documents)
      }
    }
  }

  #map calculation over all organised ap values
  for(cutoff in boundaries)
  {
    cutoff_ <- as.character(cutoff)
    mean_ap[[cutoff_]] <- list("count" = 0,"value" = 0)
    total <- 0

    #is there any value for the related cutoff
    if(length(ap[[cutoff_]])>0)
    {
      for(i in 0:(length(ap[[cutoff_]])-1))
      {
        index <- as.character(i)
        total <- total+ap[[cutoff_]][[index]]$value
      }

      mean_ap[[cutoff_]]$count <- length(ap[[cutoff_]]) #how many ap was used
      mean_ap[[cutoff_]]$value <- total/length(ap[[cutoff_]])
    }
  }

  return(mean_ap)
}

#' Geometric Mean Average Precision.
#' boundaries parameter means the cutoff points according to the total results.
#' constant parameter is used in the calculation of GMAP in order to avoid zero values during the calculation.
#'
#' @param interactions A list.
#' @param boundaries A list.
#' @param constant A double.
#' @return The results of GMAP calculation based on the boundaries parameter.
#' @export
gmap <- function(interactions, constant = 0.01, boundaries = list('all'))
{
  ap <- list()
  gmap <- list() # geometric mean ap
  total <- list()

  for(cutoff in boundaries)
  {
    ap[[as.character(cutoff)]] <- c()
  }

  for (query in interactions)
  {
    total_result <- query$total_result #list: total count of results
    related_documents <- unique(query$related_documents) #the list of documents which is known that related to the query

    #visited_documents: id of each visited page on the result list
    #the reason of using array_unique is to ignore the same page visits (happened repeatedly) in the same query
    visited_documents <- unique(query$visited_documents)

    #visited_documents_orders: the order of each visited page, which it's id is known, on the result list
    visited_documents_orders <- query$visited_documents_orders[order(unlist(query$visited_documents_orders), decreasing=FALSE)]

    for(cutoff in boundaries)
    {
      cutoff_ <- as.character(cutoff) # it is used as an index value
      pass_ <- visited_documents

      if(is.numeric(cutoff))
      {
        if(total_result>=cutoff)
        {
          index <- as.character(length(ap[[cutoff_]]))
          ap[[cutoff_]][[index]] <- list("value" = 0, "count" = 0)

          for(document in names(visited_documents_orders))
          {
            if(document %in% pass_ & document %in% related_documents & visited_documents_orders[[as.character(document)]]<=cutoff)
            {
              ap[[cutoff_]][[index]]$count <- ap[[cutoff_]][[index]]$count+1
              ap[[cutoff_]][[index]]$value <- ap[[cutoff_]][[index]]$value + ap[[cutoff_]][[index]]$count/visited_documents_orders[[as.character(document)]]

              pass_=setdiff(pass_, c(document))
            }
          }

          ap[[cutoff_]][[index]]$value <- ap[[cutoff_]][[index]]$value/length(related_documents)
        }
      }
      else
      {
        index <- as.character(length(ap[[cutoff_]]))
        ap[[cutoff_]][[index]] <- list("value" = 0, "count" = 0)

        for(document in names(visited_documents_orders))
        {
          if(document %in% pass_ & document %in% related_documents)
          {
            ap[[cutoff_]][[index]]$count <- ap[[cutoff_]][[index]]$count+1
            ap[[cutoff_]][[index]]$value <- ap[[cutoff_]][[index]]$value + ap[[cutoff_]][[index]]$count/visited_documents_orders[[as.character(document)]]

            pass_=setdiff(pass_, c(document))
          }
        }

        ap[[cutoff_]][[index]]$value <- ap[[cutoff_]][[index]]$value/length(related_documents)
      }
    }
  }

  #gmap calculation over all organised ap values
  for(cutoff in boundaries)
  {
    cutoff_ <- as.character(cutoff)
    gmap[[cutoff_]] <- list("count" = 0,"value" = 0)
    total <- 1

    #is there any value for the related cutoff
    if(length(ap[[cutoff_]])>0)
    {
      for(i in 0:(length(ap[[cutoff_]])-1))
      {
        index <- as.character(i)
        if(ap[[cutoff_]][[index]]$value!=0) { total <- total*(ap[[cutoff_]][[index]]$value+constant) }
        else { total <- total*constant }
      }

      gmap[[cutoff_]]$count <- length(ap[[cutoff_]]) #how many ap was used
      gmap[[cutoff_]]$value <- `^`(total, (1/length(ap[[cutoff_]])))
    }
  }

  return(gmap)
}

#' Eleven Point - Interpolated Average Precision.
#'
#' @param interactions A list.
#' @return The results of IAP calculation.
#' @export
iap <- function(interactions)
{
  iap <- list()
  ep <- list("0.0" = 0, "0.1" = 0, "0.2" = 0, "0.3" = 0, "0.4" = 0, "0.5" = 0, "0.6" = 0, "0.7" = 0, "0.8" = 0, "0.9" = 0, "1.0" = 0)
  p_iap <- list()

  index <- 0
  for (query in interactions)
  {
    index_ = as.character(index)
    related_documents <- unique(query$related_documents) #the list of documents which is known that related to the query

    #visited_documents: id of each visited page on the result list
    #the reason of using array_unique is to ignore the same page visits (happened repeatedly) in the same query
    visited_documents <- unique(query$visited_documents)

    #visited_documents_orders: the order of each visited page, which it's id is known, on the result list
    visited_documents_orders <- query$visited_documents_orders

    p_iap[[index_]] <- list("count" = 0, "rp" = list(), "ep" = ep)

    for(document_id in visited_documents)
    {
      if(document_id %in% related_documents)
      {
        p_iap[[index_]]$count <- p_iap[[index_]]$count+1

        # calculation is stored as recall=>precision
        recall <- as.character(p_iap[[index_]]$count / length(related_documents))
        p_iap[[index_]]$rp[[recall]] <- p_iap[[index_]]$count / visited_documents_orders[[as.character(document_id)]]
      }
    }

    p_iap[[index_]]$rp <- p_iap[[index_]]$rp[order(names(p_iap[[index_]]$rp), decreasing=TRUE)]

    bprec <- 0 #the biggest precision is going to be hold
    for(recall in names(p_iap[[index_]]$rp))
    {
      precision <- p_iap[[index_]]$rp[[recall]]
      if(bprec == 0) { bprec <- precision }

      if(bprec < precision)
      {
        bprec <- precision
        p_iap[[index_]]$rp[[recall]] <- bprec
      }
      else { p_iap[[index_]]$rp[[recall]] <- bprec }
    }

    p_iap[[index_]]$rp <- p_iap[[index_]]$rp[order(names(p_iap[[index_]]$rp), decreasing=FALSE)]

    pass_ <- c()
    for(recall in names(p_iap[[index_]]$rp))
    {
      precision <- p_iap[[index_]]$rp[[recall]]

      for(recall2 in names(p_iap[[index_]]$ep))
      {
        if(!(recall2 %in% pass_))
        {
          #options(digits=7): default is 7. if the value is low, it might need to be changed to upper values.
          if(as.double(recall2) <= as.double(recall))
          {
            p_iap[[index_]]$ep[[recall2]] <- precision
            pass_ <- append(pass_, c(recall2))
          }
          else { break }
        }
      }
    }

    index <- index+1
  }

  total <- ep
  if(length(p_iap) > 0)
  {
    for(i in 0:(length(p_iap)-1))
    {
      index <- as.character(i)

      for (recall in names(p_iap[[index]]$ep))
      {
        precision <- p_iap[[index]]$ep[[recall]]
        total[[recall]] = total[[recall]]+precision
      }
    }

    for(recall in names(total))
    {
      precision <- total[[recall]]
      iap[[recall]] <- precision / length(p_iap)
    }
  }
  else
  {
    for(recall in names(ep))
    {
      iap[[recall]] <- 0
    }
  }

  return(iap)
}

#' R-Precision.
#' boundaries parameter means the cutoff points according to the total results.
#'
#' @param interactions A list.
#' @param boundaries A list.
#' @return The results of R-Precision calculation based on the boundaries parameter.
#' @export
rprecision <- function(interactions, boundaries = list('all'))
{
  rprecision <- list()
  m_rprecision <- list() #mean rprecision

  for(cutoff in boundaries)
  {
    rprecision[[as.character(cutoff)]] <- c()
  }

  for (query in interactions)
  {
    total_result <- query$total_result #list: total count of results
    related_documents <- unique(query$related_documents) #the list of documents which is known that related to the query

    #visited_documents: id of each visited page on the result list
    #the reason of using array_unique is to ignore the same page visits (happened repeatedly) in the same query
    visited_documents <- unique(query$visited_documents)

    #visited_documents_orders: the order of each visited page, which it's id is known, on the result list
    visited_documents_orders <- query$visited_documents_orders

    for(cutoff in boundaries)
    {
      cutoff_ <- as.character(cutoff) # it is used as an index value
      if(is.numeric(cutoff))
      {
        if(total_result>=cutoff)
        {
          related <- 0

          for(document_id in visited_documents)
          {
            if(document_id %in% related_documents & visited_documents_orders[[as.character(document_id)]]<=cutoff)
            {
              related <- related + 1
            }
          }

          rprecision[[cutoff_]] <- append(rprecision[[cutoff_]], (related/length(related_documents)))
        }
      }
      else
      {
        related <- 0

        for(document_id in visited_documents)
        {
          if(document_id %in% related_documents)
          {
            related <- related + 1
          }
        }

        rprecision[[cutoff_]] <- append(rprecision[[cutoff_]], (related/length(related_documents)))
      }
    }

  }

  #mean rprecision calculation over all organised rprecision values
  for(cutoff in boundaries)
  {
    cutoff_ <- as.character(cutoff)
    m_rprecision[[cutoff_]] <- list("count" = 0,"value" = 0)

    #is there any value for the related cutoff
    if(length(rprecision[[cutoff_]])>0)
    {
      m_rprecision[[cutoff_]]$count <- length(rprecision[[cutoff_]]) #how many rprecision was used
      m_rprecision[[cutoff_]]$value <- sum(rprecision[[cutoff_]])/length(rprecision[[cutoff_]])
    }
  }

  return(m_rprecision)
}

#' F-Measure.
#' boundaries parameter means the cutoff points according to the total results.
#'
#' @param interactions A list.
#' @param boundaries A list.
#' @return The results of F-Measure calculation based on the boundaries parameter.
#' @export
fmeasure <- function(interactions, boundaries = list('all'))
{
  fs <- list()
  fmeasure <- list()

  for(cutoff in boundaries)
  {
    fs[[as.character(cutoff)]] <- c()
  }

  for (query in interactions)
  {
    total_result <- query$total_result #list: total count of results
    related_documents <- unique(query$related_documents) #the list of documents which is known that related to the query

    #visited_documents: id of each visited page on the result list
    #the reason of using array_unique is to ignore the same page visits (happened repeatedly) in the same query
    visited_documents <- unique(query$visited_documents)

    #visited_documents_orders: the order of each visited page, which it's id is known, on the result list
    visited_documents_orders <- query$visited_documents_orders

    for(cutoff in boundaries)
    {
      cutoff_ <- as.character(cutoff) # it is used as an index value
      if(is.numeric(cutoff))
      {
        if(total_result>=cutoff)
        {
          related <- 0

          for(document_id in visited_documents)
          {
            if(document_id %in% related_documents & visited_documents_orders[[as.character(document_id)]]<=cutoff)
            {
              related <- related + 1
            }
          }

          precision <- related/cutoff
          recall <- related/length(related_documents)
          p_plus_r <- precision+recall

          #2*precision*recall/precision+recall
          if(p_plus_r > 0) { fs[[cutoff_]] <- append(fs[[cutoff_]], ((2*precision*recall)/p_plus_r)) }
          else { fs[[cutoff_]] <- append(fs[[cutoff_]], 0) }
        }
      }
      else
      {
        related <- 0

        for(document_id in visited_documents)
        {
          if(document_id %in% related_documents)
          {
            related <- related + 1
          }
        }

        precision <- related/total_result
        recall <- related/length(related_documents)
        p_plus_r <- precision+recall

        #2*precision*recall/precision+recall
        if(p_plus_r > 0) { fs[[cutoff_]] <- append(fs[[cutoff_]], ((2*precision*recall)/p_plus_r)) }
        else { fs[[cutoff_]] <- append(fs[[cutoff_]], 0) }
      }
    }

  }

  #fmeasure calculation over all organised fs values
  for(cutoff in boundaries)
  {
    cutoff_ <- as.character(cutoff)
    fmeasure[[cutoff_]] <- list("count" = 0,"value" = 0)

    #is there any value for the related cutoff
    if(length(fs[[cutoff_]])>0)
    {
      fmeasure[[cutoff_]]$count <- length(fs[[cutoff_]]) #how many fs was used
      fmeasure[[cutoff_]]$value <- sum(fs[[cutoff_]])/length(fs[[cutoff_]])
    }
  }

  return(fmeasure)
}

#' Cumulative Gain.
#' boundaries parameter means the cutoff points according to the total results.
#'
#' @param interactions A list.
#' @param boundaries A list.
#' @return The results of CG calculation based on the boundaries parameter.
#' @export
cgain <- function(interactions, boundaries = list('all'))
{
  cg <- list()
  cgain <- list()

  for(cutoff in boundaries)
  {
    cg[[as.character(cutoff)]] <- c()
  }

  for (query in interactions)
  {
    total_result <- query$total_result #list: total count of results
    assessed_documents <- query$assessed_documents #the list of documents ,which is assesed by a specialist or a user, related to the query

    orders <- c()
    assessments <- c()

    for(document in names(assessed_documents))
    {
      orders <- append(orders, as.integer(assessed_documents[[document]][[1]]))
      assessments <- append(assessments, as.integer(assessed_documents[[document]][[2]]))
    }

    for(cutoff in boundaries)
    {
      cutoff_ <- as.character(cutoff) # it is used as an index value
      if(is.numeric(cutoff))
      {
        if(total_result>=cutoff)
        {
          index <- as.character(length(cg[[cutoff_]]))
          cg[[cutoff_]][[index]] <- 0

          for(i in 1:cutoff)
          {
            if(i %in% orders)
            {
              cg[[cutoff_]][[index]] <- cg[[cutoff_]][[index]] + assessments[[match(i, orders)]]
            }
          }
        }
      }
      else
      {
        index <- as.character(length(cg[[cutoff_]]))
        cg[[cutoff_]][[index]] <- 0

        for(i in 1:total_result)
        {
          if(i %in% orders)
          {
            cg[[cutoff_]][[index]] = cg[[cutoff_]][[index]] + assessments[[match(i, orders)]]
          }
        }
      }
    }
  }

  #cgain calculation over all organised cg values
  for(cutoff in boundaries)
  {
    cutoff_ <- as.character(cutoff)
    cgain[[cutoff_]] <- list("count" = 0,"value" = 0)

    #is there any value for the related cutoff
    if(length(cg[[cutoff_]])>0)
    {
      cgain[[cutoff_]]$count <- length(cg[[cutoff_]]) #how many cg was used
      cgain[[cutoff_]]$value <- sum(cg[[cutoff_]])/length(cg[[cutoff_]])
    }
  }

  return(cgain)
}

#' Normalized Cumulative Gain.
#' boundaries parameter means the cutoff points according to the total results.
#'
#' @param interactions A list.
#' @param boundaries A list.
#' @return The results of NCG calculation based on the boundaries parameter.
#' @export
ncgain <- function(interactions, boundaries = list('all'))
{
  ncgain <- list()
  ncg <- list()
  encg <- list()

  for(cutoff in boundaries)
  {
    ncg[[as.character(cutoff)]] <- c()
    encg[[as.character(cutoff)]] <- c()
  }

  for (query in interactions)
  {
    total_result <- query$total_result #list: total count of results
    assessed_documents <- query$assessed_documents #the list of documents ,which is assesed by a specialist or a user, related to the query

    orders <- c()
    assessments <- c()
    expected_assessments <- c()

    for(document in names(assessed_documents))
    {
      orders <- append(orders, as.integer(assessed_documents[[document]][[1]]))
      assessments <- append(assessments, as.integer(assessed_documents[[document]][[2]]))
      expected_assessments <- append(expected_assessments, as.integer(assessed_documents[[document]][[2]]))
    }

    expected_assessments <- expected_assessments[order(unlist(expected_assessments), decreasing=TRUE)]

    for(cutoff in boundaries)
    {
      cutoff_ <- as.character(cutoff) # it is used as an index value
      if(is.numeric(cutoff))
      {
        if(total_result>=cutoff)
        {
          index <- as.character(length(ncg[[cutoff_]]))
          ncg[[cutoff_]][[index]] <- 0
          encg[[cutoff_]][[index]] <- 0

          for(i in 1:cutoff)
          {
            if(i %in% orders)
            {
              ncg[[cutoff_]][[index]] <- ncg[[cutoff_]][[index]] + assessments[[match(i, orders)]]
            }

            if(!is.na(expected_assessments[i])) { encg[[cutoff_]][[index]] <- encg[[cutoff_]][[index]]+expected_assessments[i] }
          }
        }
      }
      else
      {
        index <- as.character(length(ncg[[cutoff_]]))
        ncg[[cutoff_]][[index]] <- 0
        encg[[cutoff_]][[index]] <- 0

        for(i in 1:total_result)
        {
          if(i %in% orders)
          {
            ncg[[cutoff_]][[index]] <- ncg[[cutoff_]][[index]] + assessments[[match(i, orders)]]
          }

          if(!is.na(expected_assessments[i])) { encg[[cutoff_]][[index]] <- encg[[cutoff_]][[index]]+expected_assessments[i] }
        }
      }
    }
  }

  #ncgain calculation over all organised ncg values
  for(cutoff in boundaries)
  {
    cutoff_ <- as.character(cutoff)
    ncgain[[cutoff_]] <- list("count" = 0,"value" = 0)
    total <- 0

    #is there any value for the related cutoff
    if(length(ncg[[cutoff_]])>0)
    {
      for(i in 0:(length(ncg[[cutoff_]])-1))
      {
        index <- as.character(i)
        total <- total+(ncg[[cutoff_]][[index]]/encg[[cutoff_]][[index]])
      }

      ncgain[[cutoff_]]$count <- length(ncg[[cutoff_]]) #how many ncg was used
      ncgain[[cutoff_]]$value <- total/length(ncg[[cutoff_]])
    }
  }

  return(ncgain)
}

#' Discounted Cumulative Gain.
#' boundaries parameter means the cutoff points according to the total results.
#'
#' @param interactions A list.
#' @param boundaries A list.
#' @return The results of DCG calculation based on the boundaries parameter.
#' @export
dcgain <- function(interactions, boundaries = list('all'))
{
  dcg <- list()
  dcgain <- list()

  for(cutoff in boundaries)
  {
    dcg[[as.character(cutoff)]] <- c()
  }

  for (query in interactions)
  {
    total_result <- query$total_result #list: total count of results
    assessed_documents <- query$assessed_documents #the list of documents ,which is assesed by a specialist or a user, related to the query

    orders <- c()
    assessments <- c()

    for(document in names(assessed_documents))
    {
      orders <- append(orders, as.integer(assessed_documents[[document]][[1]]))
      assessments <- append(assessments, as.integer(assessed_documents[[document]][[2]]))
    }

    for(cutoff in boundaries)
    {
      cutoff_ <- as.character(cutoff) # it is used as an index value
      if(is.numeric(cutoff))
      {
        if(total_result>=cutoff)
        {
          index <- as.character(length(dcg[[cutoff_]]))
          dcg[[cutoff_]][[index]] <- 0

          for(i in 1:cutoff)
          {
            if(i %in% orders)
            {
              if(i == 1) { dcg[[cutoff_]][[index]] <- dcg[[cutoff_]][[index]] + assessments[[match(i, orders)]] }
              else { dcg[[cutoff_]][[index]] <- dcg[[cutoff_]][[index]] + (assessments[[match(i, orders)]]/log2(i)) }
            }
          }
        }
      }
      else
      {
        index <- as.character(length(dcg[[cutoff_]]))
        dcg[[cutoff_]][[index]] <- 0

        for(i in 1:total_result)
        {
          if(i %in% orders)
          {
            if(i == 1) { dcg[[cutoff_]][[index]] <- dcg[[cutoff_]][[index]] + assessments[[match(i, orders)]] }
            else { dcg[[cutoff_]][[index]] <- dcg[[cutoff_]][[index]] + (assessments[[match(i, orders)]]/log2(i)) }
          }
        }
      }
    }
  }

  #dcgain calculation over all organised dcg values
  for(cutoff in boundaries)
  {
    cutoff_ <- as.character(cutoff)
    dcgain[[cutoff_]] <- list("count" = 0,"value" = 0)

    #is there any value for the related cutoff
    if(length(dcg[[cutoff_]])>0)
    {
      dcgain[[cutoff_]]$count <- length(dcg[[cutoff_]]) #how many dcg was used
      dcgain[[cutoff_]]$value <- sum(dcg[[cutoff_]])/length(dcg[[cutoff_]])
    }
  }

  return(dcgain)
}

#' Normalized Discounted Cumulative Gain.
#' boundaries parameter means the cutoff points according to the total results.
#'
#' @param interactions A list.
#' @param boundaries A list.
#' @return The results of NDCG calculation based on the boundaries parameter.
#' @export
ndcgain <- function(interactions, boundaries = list('all'))
{
  ndcgain <- list()
  dcg <- list()
  edcg <- list()

  for(cutoff in boundaries)
  {
    dcg[[as.character(cutoff)]] <- c()
    edcg[[as.character(cutoff)]] <- c()
  }

  for (query in interactions)
  {
    total_result <- query$total_result #list: total count of results
    assessed_documents <- query$assessed_documents #the list of documents ,which is assesed by a specialist or a user, related to the query

    orders <- c()
    assessments <- c()
    expected_assessments <- c()

    for(document in names(assessed_documents))
    {
      orders <- append(orders, as.integer(assessed_documents[[document]][[1]]))
      assessments <- append(assessments, as.integer(assessed_documents[[document]][[2]]))
      expected_assessments <- append(expected_assessments, as.integer(assessed_documents[[document]][[2]]))
    }

    expected_assessments <- expected_assessments[order(unlist(expected_assessments), decreasing=TRUE)]

    for(cutoff in boundaries)
    {
      cutoff_ <- as.character(cutoff) # it is used as an index value
      if(is.numeric(cutoff))
      {
        if(total_result>=cutoff)
        {
          index <- as.character(length(dcg[[cutoff_]]))
          dcg[[cutoff_]][[index]] <- 0
          edcg[[cutoff_]][[index]] <- 0

          for(i in 1:cutoff)
          {
            if(i %in% orders)
            {
              if(i == 1) { dcg[[cutoff_]][[index]] <- dcg[[cutoff_]][[index]] + assessments[[match(i, orders)]] }
              else { dcg[[cutoff_]][[index]] <- dcg[[cutoff_]][[index]] + (assessments[[match(i, orders)]]/log2(i)) }

            }

            if(i == 1) { if(!is.na(expected_assessments[i])) { edcg[[cutoff_]][[index]] <- edcg[[cutoff_]][[index]]+expected_assessments[i] } }
            else { if(!is.na(expected_assessments[i])) { edcg[[cutoff_]][[index]] <- edcg[[cutoff_]][[index]]+(expected_assessments[i]/log2(i)) } }
          }
        }
      }
      else
      {
        index <- as.character(length(dcg[[cutoff_]]))
        dcg[[cutoff_]][[index]] <- 0
        edcg[[cutoff_]][[index]] <- 0

        for(i in 1:total_result)
        {
          if(i %in% orders)
          {
            if(i == 1) { dcg[[cutoff_]][[index]] <- dcg[[cutoff_]][[index]] + assessments[[match(i, orders)]] }
            else { dcg[[cutoff_]][[index]] <- dcg[[cutoff_]][[index]] + (assessments[[match(i, orders)]]/log2(i)) }

          }

          if(i == 1) { if(!is.na(expected_assessments[i])) { edcg[[cutoff_]][[index]] <- edcg[[cutoff_]][[index]]+expected_assessments[i] } }
          else { if(!is.na(expected_assessments[i])) { edcg[[cutoff_]][[index]] <- edcg[[cutoff_]][[index]]+(expected_assessments[i]/log2(i)) } }
        }
      }
    }
  }

  #ndcgain calculation over all organised dcg values
  for(cutoff in boundaries)
  {
    cutoff_ <- as.character(cutoff)
    ndcgain[[cutoff_]] <- list("count" = 0,"value" = 0)
    total <- 0

    #is there any value for the related cutoff
    if(length(dcg[[cutoff_]])>0)
    {
      for(i in 0:(length(dcg[[cutoff_]])-1))
      {
        index <- as.character(i)
        total <- total+(dcg[[cutoff_]][[index]]/edcg[[cutoff_]][[index]])
      }

      ndcgain[[cutoff_]]$count <- length(dcg[[cutoff_]]) #how many dcg was used
      ndcgain[[cutoff_]]$value <- total/length(dcg[[cutoff_]])
    }
  }

  return(ndcgain)
}

#' Mean Reciprocal Rank.
#'
#' @param interactions A list.
#' @return The results of MRR calculation.
#' @export
mrr <- function(interactions)
{
  total_inteaction <- 0
  rr <- 0

  for (query in interactions)
  {
    # first_visit: the order number of first visited page on the result list
    first_visit <- query$visited_documents_orders[[1]][1]

    rr <- rr + (1/first_visit)
    total_inteaction <- total_inteaction+1
  }

  #mrr calculation over rr value
  mrr <- rr/total_inteaction

  return(mrr)
}

#' Rank-Biased Precision.
#' boundaries parameter means the cutoff points according to the total results.
#' p --persistence (or probability) levels-- parameter is used in the calculation of RBP.
#'
#' @param interactions A list.
#' @param boundaries A list.
#' @param p A list.
#' @return The results of RBP calculation based on the boundaries parameter.
#' @export
rbprecision <- function(interactions, p = list(0.5,0.8,0.95), boundaries = list('all'))
{
  rbprecision <- list()
  m_rbprecision <- list() # mean rbprecision

  for(cutoff in boundaries)
  {
    cutoff_ <- as.character(cutoff)
    rbprecision[[cutoff_]] <- list()
    m_rbprecision[[cutoff_]] <- list()

    for(values in p)
    {
      persistence <- as.character(values)
      rbprecision[[cutoff_]][[persistence]] <- c()
      m_rbprecision[[cutoff_]][[persistence]] <- list("count" = 0,"value" = 0)
    }
  }

  for (query in interactions)
  {
    total_result <- query$total_result #list: total count of results
    related_documents <- unique(query$related_documents) #the list of documents which is known that related to the query

    #visited_documents: id of each visited page on the result list
    #the reason of using array_unique is to ignore the same page visits (happened repeatedly) in the same query
    visited_documents <- unique(query$visited_documents)

    #visited_documents_orders: the order of each visited page, which it's id is known, on the result list
    visited_documents_orders <- query$visited_documents_orders[order(unlist(query$visited_documents_orders), decreasing=FALSE)]

    for(cutoff in boundaries)
    {
      cutoff_ <- as.character(cutoff) # it is used as an index value

      if(is.numeric(cutoff))
      {
        if(total_result>=cutoff)
        {
          for(values in p)
          {
            pass_ <- visited_documents

            persistence <- as.character(values)
            index <- as.character(length(rbprecision[[cutoff_]][[persistence]]))

            rbprecision[[cutoff_]][[persistence]][[index]] <- 0

            for(document in names(visited_documents_orders))
            {
              if(document %in% pass_ & visited_documents_orders[[as.character(document)]]<=cutoff)
              {
                if(visited_documents_orders[[as.character(document)]] == 1)
                {
                  rbprecision[[cutoff_]][[persistence]][[index]] <- rbprecision[[cutoff_]][[persistence]][[index]] + 1
                }
                else
                {
                  rbprecision[[cutoff_]][[persistence]][[index]] <- rbprecision[[cutoff_]][[persistence]][[index]] + (`^`(values, (visited_documents_orders[[as.character(document)]]-1)))
                }

                pass_=setdiff(pass_, c(document))
              }
            }

            rbprecision[[cutoff_]][[persistence]][[index]] <- rbprecision[[cutoff_]][[persistence]][[index]] * (1-values)
          }
        }
      }
      else
      {
        for(values in p)
        {
          pass_ <- visited_documents

          persistence <- as.character(values)
          index <- as.character(length(rbprecision[[cutoff_]][[persistence]]))

          rbprecision[[cutoff_]][[persistence]][[index]] <- 0

          for(document in names(visited_documents_orders))
          {
            if(document %in% pass_)
            {
              if(visited_documents_orders[[as.character(document)]] == 1)
              {
                rbprecision[[cutoff_]][[persistence]][[index]] <- rbprecision[[cutoff_]][[persistence]][[index]] + 1
              }
              else
              {
                rbprecision[[cutoff_]][[persistence]][[index]] <- rbprecision[[cutoff_]][[persistence]][[index]] + (`^`(values, (visited_documents_orders[[as.character(document)]]-1)))
              }

              pass_=setdiff(pass_, c(document))
            }
          }

          rbprecision[[cutoff_]][[persistence]][[index]] <- rbprecision[[cutoff_]][[persistence]][[index]] * (1-values)
        }
      }
    }
  }

  #mean rbprecision calculation over all organised rbprecision values
  for(cutoff in boundaries)
  {
    for(values in p)
    {
      cutoff_ <- as.character(cutoff)
      persistence <- as.character(values)

      #is there any value for the related cutoff
      if(length(rbprecision[[cutoff_]][[persistence]])>0)
      {
        m_rbprecision[[cutoff_]][[persistence]]$count <- length(rbprecision[[cutoff_]][[persistence]]) #how many rbprecision was used
        m_rbprecision[[cutoff_]][[persistence]]$value <- sum(rbprecision[[cutoff_]][[persistence]])/length(rbprecision[[cutoff_]][[persistence]])
      }
    }
  }

  return(m_rbprecision)
}

#' Expected Reciprocal Rank.
#' boundaries parameter means the cutoff points according to the total results.
#' max_grade parameter is used in the calculation of ERR.
#' The minimum level is also used in the calculation but does not need to be included as a parameter for this measurement.
#'
#' @param interactions A list.
#' @param boundaries A list.
#' @param max_grade An integer.
#' @return The results of ERR calculation based on the boundaries parameter.
#' @export
err <- function(interactions, max_grade = 5, boundaries = list('all'))
{
  err <- list()
  m_err <- list() # mean err

  for(cutoff in boundaries)
  {
    err[[as.character(cutoff)]] <- c()
  }

  for (query in interactions)
  {
    total_result <- query$total_result #list: total count of results
    assessed_documents <- query$assessed_documents #the list of documents ,which is assesed by a specialist or a user, related to the query

    orders <- c()
    assessments <- c()

    for(document in names(assessed_documents))
    {
      orders <- append(orders, as.integer(assessed_documents[[document]][[1]]))
      assessments <- append(assessments, as.integer(assessed_documents[[document]][[2]]))
    }

    for(cutoff in boundaries)
    {
      cutoff_ <- as.character(cutoff) # it is used as an index value
      if(is.numeric(cutoff))
      {
        if(total_result>=cutoff)
        {
          temporary <- list("rank" = c(), "pvalue" = c())
          index <- as.character(length(err[[cutoff_]]))
          err[[cutoff_]][[index]] <- 0

          for(i in 1:cutoff)
          {
            if(i %in% orders)
            {
              temporary$rank <- append(temporary$rank, 1/i)
              temporary$pvalue <- append(temporary$pvalue, ((`^`(2, assessments[[match(i, orders)]]))-1)/(`^`(2, max_grade)))
            }
          }

          if(length(temporary$rank)>0)
          {
            for(i in length(temporary$rank):1)
            {
              others <- 1

              if(i>1)
              {
                for(j in (i-1)) { others <- others * (1-temporary$pvalue[[j]]) }
              }

              err[[cutoff_]][[index]] <- err[[cutoff_]][[index]] + (temporary$rank[[i]] * temporary$pvalue[[i]] * others)
            }
          }
        }
      }
      else
      {
        temporary <- list("rank" = c(), "pvalue" = c())
        index <- as.character(length(err[[cutoff_]]))
        err[[cutoff_]][[index]] <- 0

        for(i in 1:total_result)
        {
          if(i %in% orders)
          {
            temporary$rank <- append(temporary$rank, 1/i)
            temporary$pvalue <- append(temporary$pvalue, ((`^`(2, assessments[[match(i, orders)]]))-1)/(`^`(2, max_grade)))
          }
        }

        if(length(temporary$rank)>0)
        {
          for(i in length(temporary$rank):1)
          {
            others <- 1

            if(i>1)
            {
              for(j in (i-1)) { others <- others * (1-temporary$pvalue[[j]]) }
            }

            err[[cutoff_]][[index]] <- err[[cutoff_]][[index]] + (temporary$rank[[i]] * temporary$pvalue[[i]] * others)
          }
        }
      }
    }
  }

  #m_err calculation over all organised err values
  for(cutoff in boundaries)
  {
    cutoff_ <- as.character(cutoff)
    m_err[[cutoff_]] <- list("count" = 0,"value" = 0)

    #is there any value for the related cutoff
    if(length(err[[cutoff_]])>0)
    {
      m_err[[cutoff_]]$count <- length(err[[cutoff_]]) #how many err was used
      m_err[[cutoff_]]$value <- sum(err[[cutoff_]])/length(err[[cutoff_]])
    }
  }

  return(m_err)
}

#' BPref.
#' boundaries parameter means the cutoff points according to the total results.
#'
#' @param interactions A list.
#' @param boundaries A list.
#' @return The results of BPref calculation based on the boundaries parameter.
#' @export
bpref <- function(interactions, boundaries = list('all'))
{
  bpref <- list()
  m_bpref <- list() # mean bpref

  for(cutoff in boundaries)
  {
    bpref[[as.character(cutoff)]] <- c()
  }

  for (query in interactions)
  {
    total_result <- query$total_result #list: total count of results
    assessed_documents <- query$assessed_documents #the list of documents ,which is assesed by a specialist or a user, related to the query
    related_documents <- c()

    orders <- c()
    assessments <- c()

    for(document in names(assessed_documents))
    {
      orders <- append(orders, as.integer(assessed_documents[[document]][[1]]))
      assessments <- append(assessments, as.integer(assessed_documents[[document]][[2]]))

      if(assessed_documents[[document]][[2]] == 1) { related_documents <- append(related_documents, document) }
    }

    for(cutoff in boundaries)
    {
      cutoff_ <- as.character(cutoff) # it is used as an index value
      if(is.numeric(cutoff))
      {
        if(total_result>=cutoff)
        {
          index <- as.character(length(bpref[[cutoff_]]))

          if(length(related_documents) > 0)
          {
            bpref[[cutoff_]][[index]] <- (1/length(related_documents))
            temp_calc <- 0

            for(order_ in orders)
            {
              if(order_ <= cutoff)
              {
                counter <- 0

                if(assessments[[which(orders == order_)]] == 1)
                {
                  for(order_2 in orders)
                  {
                    if(order_2 < order_)
                    {
                      if(assessments[[which(orders == order_2)]] == 0) { counter <- counter + 1 }
                    }
                    else { break; }
                  }

                  temp_calc <- temp_calc + (1-(counter/length(related_documents)))
                }
              }
              else { break; }
            }

            bpref[[cutoff_]][[index]] <- bpref[[cutoff_]][[index]]  * temp_calc
          }
          else { bpref[[cutoff_]][[index]] <- 0 }
        }
      }
      else
      {
        index <- as.character(length(bpref[[cutoff_]]))

        if(length(related_documents) > 0)
        {
          bpref[[cutoff_]][[index]] <- (1/length(related_documents))
          temp_calc <- 0

          for(order_ in orders)
          {
            counter <- 0

            if(assessments[[which(orders == order_)]] == 1)
            {
              for(order_2 in orders)
              {
                if(order_2 < order_)
                {
                  if(assessments[[which(orders == order_2)]] == 0) { counter <- counter + 1 }
                }
                else { break; }
              }

              temp_calc <- temp_calc + (1-(counter/length(related_documents)))
            }
          }

          bpref[[cutoff_]][[index]] <- bpref[[cutoff_]][[index]]  * temp_calc
        }
        else { bpref[[cutoff_]][[index]] <- 0 }
      }
    }
  }

  #mean bpref calculation over all organised bpref values
  for(cutoff in boundaries)
  {
    cutoff_ <- as.character(cutoff)
    m_bpref[[cutoff_]] <- list("count" = 0,"value" = 0)
    total <- 0

    #is there any value for the related cutoff
    if(length(bpref[[cutoff_]])>0)
    {
      for(i in 0:(length(bpref[[cutoff_]])-1))
      {
        index <- as.character(i)
        total <- total+bpref[[cutoff_]][[index]]
      }

      m_bpref[[cutoff_]]$count <- length(bpref[[cutoff_]]) #how many bpref was used
      m_bpref[[cutoff_]]$value <- total/length(bpref[[cutoff_]])
    }
  }

  return(m_bpref)
}
