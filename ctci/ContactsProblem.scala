object ContactsProblem extends App {
	class Node {
		private val NumberOfCharacters: Int = 26
		private val children: Array[Node] = Array.ofDim[Node](26)
		private var size: Int = 0

		private def getCharIndex(c: Char): Int = {
			c - 'a'
		}

		private def getNode(c: Char): Node = {
			val idx: Int = getCharIndex(c)
			children(idx)
		}

		private def setNode(c: Char, node: Node): Unit = {
			val idx: Int = getCharIndex(c)
			children(idx) = node
		}

		def add(s: String): Unit = {
			add(s, 0)
		}

		private def add(s: String, index: Int): Unit = {
			size += 1
			if (index == s.length) {
				()
			}

			else {
				val current: Char = s.charAt(index)
				var child: Node = getNode(current)
				if (child == null) {
				 	child = new Node
				 	setNode(current, child)
				 } 
				child.add(s, index + 1) 
			}
		}

		def findCount(prefix: String): Int = {
			findCount(prefix, 0)
		}

		private def findCount(prefix: String, index: Int): Int = {
			if (index == prefix.length) {
				size 
			}

			else {
				val prefixLetter: Char = prefix.charAt(index)
				val child: Node = getNode(prefixLetter)

				if (child == null) {
					0
				}

				else {
					child.findCount(prefix, index + 1)
				}
			}

		}

	}

	val trie: Node = new Node
	trie.add("gayle")
	trie.add("gary")
	trie.add("genna")
	trie.add("alex")
	trie.add("andy")
	trie.add("gambles")

	println(trie.findCount("ga"))
}