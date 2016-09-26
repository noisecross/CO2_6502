/**
* |------------------------------------------|
* | CO2 6502, COMPILER OPTIMIZER TO 6502     |
* | File: feErrMsg.h                         |
* | v1.0, September 2012                     |
* | Author: Emilio Arango Delgado de Mendoza |
* |------------------------------------------|
*/

/**
* Front end possible error messages
*/
#ifndef FeErrMsgH
#define FeErrMsgH

#define ERR_LEX_MSG_00 "S�mbolo no reconocido en el lenguaje"
#define ERR_LEX_MSG_01 "Nombre de identificador demasiado largo"
#define ERR_LEX_MSG_02 "Fin de fichero encontrado dentro de un comentario"

#define ERR_SIN_MSG_00 "Funcionalidad todavia no desarrollada"
#define ERR_SIN_MSG_01 "Operacion no soportada por la arquitectura 6502"
#define ERR_SIN_MSG_02 "Se esperaba especificador de tipo"

#define ERR_SEM_MSG_00 "Identificador no declarado"
#define ERR_SEM_MSG_01 "Par�metros de entrada incompatibles con la funci�n"
#define ERR_SEM_MSG_02 "Funci�n no declarada"
#define ERR_SEM_MSG_03 "Tipo no declarado"
#define ERR_SEM_MSG_04 "El tipo no se puede promocionar"
#define ERR_SEM_MSG_05 "La operaci�n solamente es v�lida para tipos de 1 byte de tama�o"
#define ERR_SEM_MSG_06 "Tipos incompatibles"
#define ERR_SEM_MSG_07 "Error de tipo en la declaraci�n de la variable"
#define ERR_SEM_MSG_08 "Una variable no admite la propiedad \'interrupt\'"
#define ERR_SEM_MSG_09 "Variable previamente declarada"
#define ERR_SEM_MSG_0A "No se encuentra la funci�n de entrada \'main\'"
#define ERR_SEM_MSG_0B "Redefinici�n de una funci�n ya declarada"
#define ERR_SEM_MSG_0C "Redefinici�n de una etiqueta en el mismo espacio de nombres"
#define ERR_SEM_MSG_0D "La sentencia no puede utilizarse en ese punto del c�digo"
#define ERR_SEM_MSG_0E "La funci�n \'main\' no admite argumentos de entrada"
#define ERR_SEM_MSG_0F "El literal no puede ser convertido a d�gito"
#define ERR_SEM_MSG_10 "Fallo durante el an�lisis sem�ntico. Error en el compilador"
#define ERR_SEM_MSG_11 "Operaci�n no v�lida para tipos sin signo"
#define ERR_SEM_MSG_12 "Etiqueta no definida en esta funci�n"
#define ERR_SEM_MSG_13 "Asignaci�n sobre una variable definida como \'const\'"
#define ERR_SEM_MSG_14 "Desbordamiento en la asignaci�n"
#define ERR_SEM_MSG_15 "Funci�n declarada pero no definida"
#define ERR_SEM_MSG_16 "El compilador no admite grafos cerrados en las llamadas a funciones."
#define ERR_SEM_MSG_17 ". Trate de utilizar la flag -r (el rendimiento puede verse reducido)." 

#define WAR_SEM_MSG_00 "El valor ser� siempre 0"
#define WAR_SEM_MSG_01 "Operaci�n entre tipos con y sin signo"
#define WAR_SEM_MSG_02 "Existen sentencias tras un \'return\' obligatorio"
#define WAR_SEM_MSG_03 "No todas las rutas de acceso de control devuelven un valor"
#define WAR_SEM_MSG_04 "Bucle definido incorrecto. La condici�n inicial no supera la prueba"
#define WAR_SEM_MSG_05 "Punteros de diferente profundidad. �Olvid� hacer un cast?"
#define WAR_SEM_MSG_06 "Perdida de informaci�n al promocionar un tipo"
#define WAR_SEM_MSG_07 "Variable declarada pero no utilizada "

#endif
