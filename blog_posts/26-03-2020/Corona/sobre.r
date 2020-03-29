function(){
  
	tabPanel("Sobre",
		HTML('<div style="float: right; margin: 0 5px 5px 10px;">
          <p style="text-align:justify", encoding = "UTF-8">
Este aplicativo Shiny foi produzido a partir dos dados do site <a href="https://brasil.io/dataset/covid19/caso" target="_blank">Brasil.io</a> </p><br/>'),

		
		
		HTML('
		<p>Prof. Fernando de Souza Bastos<br/>
		Doutor em Estatistica | useR<br/>
		<a href="https://github.com/fsbmat" target="_blank">Github.io</a> | 
		<a href="http://fsbmat-ufv.github.io" target="_blank">Blog</a> | 
		<a href="https://twitter.com/fsbmat" target="_blank">Twitter</a> | 
		<a href="http://www.linkedin.com/in/fsbmat" target="_blank">Linkedin</a> <br/>
		</p>'),
		
		# fluidRow(
		# 	column(4,
		# 		strong('Referencias'),
		# 		p(HTML('<ul>'),
		# 			HTML('<li>'),a('R', href="http://www.r-project.org/", target="_blank"),HTML('</li>'),
		# 			HTML('<li>'),a("Shiny", href="http://www.rstudio.com/shiny/", target="_blank"),HTML('</li>'),
		# 			HTML('<li>'),a('VGAM', href="http://cran.r-project.org/web/packages/VGAM/index.html", target="_blank"),HTML('</li>'),
		# 		HTML('</ul>'))
		# 	)
		# ),
		value="sobre"
	)
}
