from pyspark.sql.types import *
import os
from pyspark.sql.functions import udf

BERKA_DIR="./spark-ejemplo/data_berka"

############################################# Client
# Definimos esquema para client
client_schema = StructType([StructField("client", IntegerType(), False)
                            , StructField("birth_number", StringType(), True)
                            , StructField("district", IntegerType(), False)])
client_schema


clients = spark.read.csv(os.path.join(BERKA_DIR, "client.asc")
                         , header=True
                         , sep=";"
                         , schema=client_schema)
clients.printSchema()
clients.show()

# Defino una función de python
def get_sex(birth_number):
    month = int(birth_number[2:4])
    sex = 'M'
    if month > 12:
        sex = 'F'
    return sex
# La aviento a spark definiendo el tipo del output
get_sex_udf = udf(get_sex, StringType())
# Defino la transformación y la acción
clients.withColumn("sex", get_sex_udf(clients.birth_number)).show()

def get_month(birth_number):
    month = int(birth_number[2:4])
    if month > 12:
        month = month - 50
    return month
get_month_udf = udf(get_month, IntegerType())

# Anido las transformaciones a los datos y genero una acción al final para ver el resultado
clients.withColumn("sex", get_sex_udf(clients.birth_number))\
.withColumn("birth_month", get_month_udf(clients.birth_number)).show()

# Guardo las puras transformaciones (ejecuté?)
clients_cleaned = clients.withColumn("sex", get_sex_udf(clients.birth_number))\
.withColumn("birth_month", get_month_udf(clients.birth_number)).drop("birth_number")
clients_cleaned.show()

############################################# Demographic data: truco coqueto para sobreescribir headers incorrectos
# Defino el esquema
district_schema = StructType([StructField("district", IntegerType(), False)
                              , StructField("name", StringType(), True)
                              , StructField("region", StringType(), True)
                              , StructField("inhabitants", IntegerType(), True)
                              , StructField("munis.inhab_0-499", IntegerType(), True)
                              , StructField("munis.inhab_500-1999", IntegerType(), True)
                              , StructField("munis.inhab_2000-9999", IntegerType(), True)
                              , StructField("munis.inhab_10000+", IntegerType(), True)
                              , StructField("cities", IntegerType(), True)
                              , StructField("urban.inhab.ratio", DoubleType(), True)
                              , StructField("salary_mean", DoubleType(), True)                              
])

districts = spark.read.csv(os.path.join(BERKA_DIR, "district.asc"), header=True, sep=";", schema=district_schema)
districts.show()


############################################# Join
############################################# Para generar las tablas temporales estilo Berka IV
clients_cleaned.createOrReplaceTempView("clients")
spark.sql("select * from clients limit 5").show()
districts.createOrReplaceTempView("districts")

############################################# Join
tabla_final = spark.sql("select c.client, c.sex, c.birth_month, d.* from clients c join districts d on c.district = d.district ")
tabla_final.show()

## Guardamos en parquet
tabla_final.write.parquet("./spark-ejemplo/output/parquet/transacciones_denormalizado", mode="overwrite")

############################################ Para generar la tabla final estilo spark
#tabla_final = clients_cleaned.join(districts, on="district").join(disposition, on="client")
tabla_final = clients_cleaned.join(districts, on="district").show()
