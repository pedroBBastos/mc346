# explicacao do *args -> dentro da funcao, args sera a lista com os n argumentos passados. *args denota que eh uma variavel que contem todos os argumentos passados
# para passar este argumentos para outra funcao, passar com o '*'. Se quero iterar sobre os argumentos, usar a lista args (sem *)
# https://www.geeksforgeeks.org/args-kwargs-python/

import time

# decorator para imprimir o tempo de execucao
def decShowTimeExecution(f):
    def wrapper(*args):
        start = time.perf_counter()
        result = f(*args)
        end = time.perf_counter()
        print("Tempo de execução -> {} segundos".format(end-start))
        return result
    return wrapper

@decShowTimeExecution
def aux(x,y):
   return 2*x+y

#decorator para construir um string com linhas para a hora e argumentos e saida de cada chamada da funcao. O string sera acessado via atributo
class decLogString:
    def __init__(self, f):
        self.logString = ""
        self.f = f
    def __call__(self, *args):
        x = self.f(*args)
        lt = time.localtime()
        self.logString += "Executando funcao às {}:{}:{}, com argumentos [".format(lt.tm_hour, lt.tm_min, lt.tm_sec)
        for a in args:
            self.logString += str(a) + ", "
        self.logString += "], com retorno => {}\n".format(x)

# decorator para memoizar a funcao. Memoizacao é criar um dicionario que se lembra dos valores de entrada e de saida da funcao ja executado.
# Se um desses valores de entrada for re-executado, a funcao nao sera re-executada - ela apenas retorna o valor de saida memoizado
class decMemoizacao:
    def __init__(self, f):
        self.dictionary = {}
        self.f = f
    def __call__(self, *args):
        if args in self.dictionary:
            print("Ja tem sabagaca")
            return self.dictionary[args]
        else:
            x = self.f(*args)
            self.dictionary[args] = x
            return x

# decorator para log argumentos e horario num arquivo (append no arquivo) dado como argumento do decorator (ver o primer on decorators)
class decAppendLogOnFile:
    def __init__(self, f, fileName):
        self.f = f
        self.fileName = fileName
    def __call__(self, *args):
        x = self.f(*args)
        lt = time.localtime()
        logString = "Executando funcao às {}:{}:{}, com argumentos [".format(lt.tm_hour, lt.tm_min, lt.tm_sec)
        for a in args:
            logString += str(a) + ", "
        logString += "], com retorno => {}\n".format(x)
        fileMio = open(self.fileName, 'a')
        fileMio.write(logString)
        fileMio.close()
