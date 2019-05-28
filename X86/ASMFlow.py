class FlowType(object):
	def get_successors(self):
		pass

class FlowCallDirect(FlowType):
	def __init__(self,target,retaddr):
		self.target = target
		self.retaddr = retaddr
	
	def get_successors(self):
		return ([self.retaddr],[self.target])

class FlowJmpUnconditional(FlowType):
	def __init__(self,target,retaddr):
		self.target = target
	
	def get_successors(self):
		return ([self.target],[])

class FlowJmpConditional(FlowType):
	def __init__(self,target,fallthrough):
		self.target = target
		self.fallthrough = fallthrough
	
	def get_successors(self):
		return ([self.target,self.fallthrough],[])

class FlowCallIndirect(FlowType):
	def __init__(self,fallthrough):
		self.retaddr = fallthrough
	
	def get_successors(self):
		return ([self.fallthrough],[])

class FlowIndirectJmp(FlowType):
	def get_successors(self):
		return ([],[])

class FlowReturn(FlowType):
	def get_successors(self):
		return ([],[])

class FlowOrdinary(FlowType):
	def __init__(self,passthrough):
		self.passthrough = passthrough
	def get_successors(self):
		return ([self.passthrough],[])

