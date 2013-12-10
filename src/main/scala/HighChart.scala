package jp.co.guru.PostgreSQLAnalyze

import Parse.ResultIterator

object HighChart {
  def main(args: Array[String]) {
    println(page)
  }
  
  val ri = new ResultIterator(Data.optSecond).toList
  def labels() = ri.map(p => s""""${p.op} ${p.desc.mkString(",")}"""").mkString(",\n")
  def periods() = ri.map(p => s"[ ${p.actualTime.start}, ${p.actualTime.end} ]").mkString(",\n")
    
  val page = 
  s"""<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" "xhtml11.dtd">
<html debug="true">
<head>
<base href="http://www.highcharts.com" />

<title>Highcharts - Column range</title>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1.0">

<link rel="shortcut icon" href="../favicon.ico"/>

<link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.2/css/bootstrap-combined.no-icons.min.css" rel="stylesheet">
<link href="//netdna.bootstrapcdn.com/font-awesome/3.2.1/css/font-awesome.css" rel="stylesheet">

<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js"></script>
<script type="text/javascript">
jQuery.noConflict();
</script>

<!-- Highslide code -->
<script type="text/javascript" src="highslide/highslide-full.min.js"></script>
<script type="text/javascript" src="highslide/highslide.config.js" charset="utf-8"></script>
<link rel="stylesheet" type="text/css" href="highslide/highslide.css" />
<!--[if lt IE 7]>
<link rel="stylesheet" type="text/css" href="/highslide/highslide-ie6.css" />
<![endif]-->
<!-- End Highslide code -->

<script src="demo/scripts.js"></script>

<script type="text/javascript">

(function($$){ // encapsulate jQuery

$$(function () {
    	
	$$('#container').highcharts({
	
	    chart: {
	        type: 'columnrange',
	        inverted: true
	    },
	    
	    title: {
	        text: 'Query Analyze'
	    },
	    
		subtitle: {
	        text: 'Opt Second'
	    },
	
	    xAxis: {
//	        categories: ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
	        categories: [ ${labels()} ]
	    },
	    
	    yAxis: {
	        title: {
	            text: 'Actual Time ( mS )'
	        }
	    },
	
	    tooltip: {
	        valueSuffix: 'mS'
	    },
	    
	    plotOptions: {
	        columnrange: {
	        	dataLabels: {
	        		enabled: true,
	        		formatter: function () {
	        			return this.y + 'mS';
	        		}
	        	}
	        }
	    },
	    
	    legend: {
	        enabled: false
	    },
	
	    series: [{
	        name: 'Temperatures',
	        data: [
                ${ periods() }
                /*
				[-9.7, 9.4],
				[-8.7, 6.5],
				[-3.5, 9.4],
				[-1.4, 19.9],
				[0.0, 22.6],
				[2.9, 29.5],
				[9.2, 30.7],
				[7.3, 26.5],
				[4.4, 18.0],
				[-3.1, 11.4],
				[-5.2, 10.4],
				[-13.5, 9.8]
                */
			]
	    }]
	
	});   
});
})(jQuery);
</script>

</head>
<body>

<div id="demo-content">
	
	<div style="margin: 5px">
	<script src="http://code.highcharts.com/highcharts.js"></script>
<script src="http://code.highcharts.com/highcharts-more.js"></script>
<script src="http://code.highcharts.com/modules/exporting.js"></script>

<div id="container" style="min-width: 310px; height: 800px; margin: 0 auto"></div>
	</div>
</div>

<script src="http://code.highcharts.com/themes/grid.js"></script>

</body>

</html>
"""
}