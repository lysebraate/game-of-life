(def block [
	[0 0 0 0]
	[0 1 1 0]
	[0 1 1 0]
	[0 0 0 0]])

(def blinker [
	[0 0 0 0 0]
	[0 0 1 0 0]
	[0 0 1 0 0]
	[0 0 1 0 0]
	[0 0 0 0 0]])

(def boat [	
	[0 0 0 0 0]
	[0 1 1 0 0]
	[0 1 0 1 0]
	[0 0 1 0 0]
	[0 0 0 0 0]])

(def beehive [
	[0 0 0 0 0 0]
	[0 0 1 1 0 0]
	[0 1 0 0 1 0]
	[0 0 1 1 0 0]
	[0 0 0 0 0 0]])

(def loaf [
	[0 0 0 0 0 0]
	[0 0 1 1 0 0]
	[0 1 0 0 1 0]
	[0 0 1 0 1 0]
	[0 0 0 1 0 0]
	[0 0 0 0 0 0]])

(def toad [
	[0 0 0 0 0 0]
	[0 0 0 0 0 0]
	[0 0 1 1 1 0]
	[0 1 1 1 0 0]
	[0 0 0 0 0 0]
	[0 0 0 0 0 0]])

(def beacon [
	[0 0 0 0 0 0]
	[0 1 1 0 0 0]
	[0 1 0 0 0 0]
	[0 0 0 0 1 0]
	[0 0 0 1 1 0]
	[0 0 0 0 0 0]])

(def pulsar [
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0]
	[0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0]
	[0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0]
	[0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0]
	[0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0]
	[0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0]
	[0 0 1 0 0 0 0 1 0 1 0 0 0 0 1 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])

(def glider [
	[0 0 0 0 0 0 0]
	[0 0 0 1 0 0 0]
	[0 0 0 0 1 0 0]
	[0 0 1 1 1 0 0]
	[0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0]])

(def exploder [
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 1 0 1 0 1 0 0 0 0 0]
	[0 0 0 0 0 1 0 0 0 1 0 0 0 0 0]
	[0 0 0 0 0 1 0 0 0 1 0 0 0 0 0]
	[0 0 0 0 0 1 0 0 0 1 0 0 0 0 0]
	[0 0 0 0 0 1 0 1 0 1 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])

(def cross [
	[0 0 0 0 0 1 0 0 0 0 0]
	[0 0 0 0 0 1 0 0 0 0 0]
	[0 0 0 0 0 1 0 0 0 0 0]
	[0 0 0 0 0 1 0 0 0 0 0]
	[0 0 0 0 0 1 0 0 0 0 0]
	[1 1 1 1 1 1 1 1 1 1 1]
	[0 0 0 0 0 1 0 0 0 0 0]
	[0 0 0 0 0 1 0 0 0 0 0]
	[0 0 0 0 0 1 0 0 0 0 0]
	[0 0 0 0 0 1 0 0 0 0 0]
	[0 0 0 0 0 1 0 0 0 0 0]])

(def r-pentamino [
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 1 1 0 0 0 0 0 0]
	[0 0 0 0 0 0 1 1 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 1 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])

(def gosper-glider-gun [
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 1 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 1 1 1 0 0 0 0 0 1 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 1 1 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 0]
	[0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
	[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])
