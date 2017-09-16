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
