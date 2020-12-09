# Importação das bibliotecas
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import confusion_matrix, accuracy_score
from sklearn.ensemble import RandomForestClassifier
import matplotlib.pyplot as plt
import numpy as np
from pandas import DataFrame
import seaborn as sn
from keras.utils import np_utils


df = pd.read_csv('G:/My Drive/Estatística/Dados/Cluster_Final/K&G/Anual/Dados/ETP_ANUAL.csv',encoding = 'utf_8')
#df = df.loc[df['Estacao'] == "Inverno"]
df = df.loc[df['NOAA'] == "PADRAO"]

names = df.columns
df.head()


previsores1 = pd.DataFrame(df.iloc[:,1:29].values)
previsores2 = pd.DataFrame(df.iloc[:,31].values)
cidades = pd.DataFrame(df.iloc[:,32].values)
anos = pd.DataFrame(df.iloc[:,0].values)
previsores = pd.concat([previsores1, previsores2,cidades,anos], axis=1)

names = df.columns[1:29].tolist() + [df.columns[31],df.columns[32],df.columns[0]]
names_plot = df.columns[1:29].tolist() + [df.columns[31]]
previsores

previsores = previsores.values

# Transformação dos atributos categóricos em atributos numéricos, passando o índice de cada coluna categórica

labelencoder1 = LabelEncoder()
labelencoder1 = LabelEncoder()

previsores[:,28] = labelencoder1.fit_transform(previsores[:,28])

previsores

classe = df.iloc[:,30].values
classe
labelencoder2 = LabelEncoder()
classe = labelencoder2.fit_transform(classe)
classe
quantidade2 = np.unique(classe, return_counts = True)
quantidade2

# Transformação da classe para o formato "dummy", pois temos uma rede neural com 3 neurônios na camada de saída
classe_dummy = np_utils.to_categorical(classe)
classe_dummy

# Divisão da base de dados entre treinamento e teste (30% para testar e 70% para treinar)
X_treinamento, X_teste, y_treinamento, y_teste = train_test_split(previsores,
                                                                  classe_dummy,
                                                                  test_size = 0.3,
                                                                  random_state = 0)
                                                                  
cidades_nomes =  X_teste[:, [29]]
anos_nomes =  X_teste[:, [30]]
anos = DataFrame(anos_nomes,columns = ["Anos"])
cidades = DataFrame(cidades_nomes,columns = ["Cidade"])
X_teste = np.delete(X_teste, np.s_[29], axis=1)
X_treinamento = np.delete(X_treinamento, np.s_[29], axis=1)
X_teste = np.delete(X_teste, np.s_[29], axis=1)
X_treinamento = np.delete(X_treinamento, np.s_[29], axis=1)

X_treinamento

# Criação do modelo, treinamento, obtenção das previsões e da taxa de acerto
floresta = RandomForestClassifier(n_estimators = 1000)
floresta.fit(X_treinamento, y_treinamento)

importancias = floresta.feature_importances_
importancias

def plot_feature_importance(importance,names,model_type):

        #Create arrays from feature importance and feature names
        feature_importance = np.array(importance)
        feature_names = np.array(names)

        #Create a DataFrame using a Dictionary
        data={'feature_names':feature_names,'feature_importance':feature_importance}
        fi_df = pd.DataFrame(data)

        #Sort the DataFrame in order decreasing feature importance
        fi_df.sort_values(by=['feature_importance'], ascending=False,inplace=True)

        #Define size of bar plot
        plt.figure(figsize=(10,8))
        #Plot Searborn bar chart
        sn.barplot(x=fi_df['feature_importance'], y=fi_df['feature_names'],palette="colorblind")
        
        #Add chart labels
        plt.title(model_type + 'FEATURE IMPORTANCE')
        plt.xlabel('FEATURE IMPORTANCE')
        plt.ylabel('FEATURE NAMES')
        
plot_feature_importance(floresta.feature_importances_,names_plot,'RANDOM FOREST ')

# Como é um problema com três saídas, precisamos buscar a posição que possui o maior valor (são retornados 3 valores)
previsoes = floresta.predict(X_teste)
y_teste_matrix = [np.argmax(t) for t in y_teste]
y_previsao_matrix = [np.argmax(t) for t in previsoes]

confusao = confusion_matrix(y_teste_matrix, y_previsao_matrix)
confusao

taxa_acerto = accuracy_score(y_teste, previsoes)
taxa_acerto

Previsao = DataFrame(y_previsao_matrix,columns=['Previsao'])
Testemunha = DataFrame(y_teste_matrix,columns=['Testemunha'])
Previsao = DataFrame(labelencoder2.inverse_transform(y_previsao_matrix),columns=['Previsao'])
Testemunha = DataFrame(labelencoder2.inverse_transform(y_teste_matrix),columns=['Testemunha'])
df_classificacao = pd.concat([Previsao, Testemunha,cidades,anos], axis=1)
df_classificacao
df_classificacao.to_csv(r'G:/My Drive/Estatística/Dados/Cluster_Final/K&G/Anual/Dados/Tabela_Comparacao_rf.csv')

df_cm = pd.DataFrame(confusao,index = sorted(pd.unique(Testemunha["Testemunha"])),columns = sorted(pd.unique(Testemunha["Testemunha"])))
df_cm
plt.figure(figsize = (10,7))
sn.heatmap(df_cm, annot=True,fmt='g')




