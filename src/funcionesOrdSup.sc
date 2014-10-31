object funcionesOrdSup {
	
	var o:Option[Int]= Some(3)                //> o  : Option[Int] = Some(3)
	val o1:Option[Int]= None                  //> o1  : Option[Int] = None

	def flatmapear[T,R](o:Option[T])(f:T=>Option[R]):Option[R]={
		o.foldLeft(None:Option[R])((_,x) => f(x))
	}                                         //> flatmapear: [T, R](o: Option[T])(f: T => Option[R])Option[R]
	
	
	
	
	
}