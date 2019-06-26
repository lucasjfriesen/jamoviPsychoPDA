
print.sensSpecTable <- function(Title, TP, FP, TN, FN){
  res <- paste0(
  "<style type='text/css'>
  .tg  {border-collapse:collapse;border-spacing:0;border-width:1px;border-style:solid;border-color:black;}
  .tg td{font-family:Arial, sans-serif;font-size:14px;padding:10px 5px;border-style:solid;border-width:0px;overflow:hidden;word-break:normal;}
  .tg th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 5px;border-style:solid;border-width:0px;overflow:hidden;word-break:normal;}
  .tg .tg-s6z2{text-align:center}
  .tg .tg-uys7{border-color:inherit;text-align:center}
  .tg .tg-h0x1{text-align:center}
  </style>
  <table class='tg'>
    <tr>
      <th class='tg-0lax' colspan='4'>",Title,"</th>
    </tr>
    <tr>
      <td class='tg-s6z2'></td>
      <td class='tg-uys7' colspan='3'>DECISION BASED ON MEASURE</td>
    </tr>
    <tr>
      <td class='tg-h0x1' rowspan='3'>CRITERION</td>
      <td class='tg-h0x1'></td>
      <td class='tg-h0x1'>Negative</td>
      <td class='tg-h0x1'>Positive</td>
    </tr>
    <tr>
      <td class='tg-s6z2'>Negative</td>
      <td class='tg-s6z2'>",TN," (TN)</td>
      <td class='tg-s6z2'>", FP," (FP)</td>
    </tr>
    <tr>
      <td class='tg-h0x1'>Positive</td>
      <td class='tg-h0x1'>", FN," (FN)</td>
      <td class='tg-h0x1'>", TP," (TP)</td>
    </tr>
    <tr>
      <td class='tg-tf2e'></td>
      <td class='tg-tf2e'></td>
      <td class='tg-tf2e'></td>
      <td class='tg-tf2e'></td>
    </tr>
</table>")
  return(res)
}

formatter <- function(x) {
          resToReturn = numeric()
          for (i in 1:length(x)) {
            number = round(x[i], 2)
            if (length(as.character(number)) <= 3){
              resToReturn[[i]] <- jmvcore::format("{}.00%", number)
            }
              resToReturn[[i]] <- jmvcore::format("{}%", number)
            }
          resToReturn
        }

