from pulp import *
import pandas as pd

food_data = pd.read_excel(r"data 15.2\diet.xls")

food_data = food_data[:64]

food_data = food_data.values.tolist()

foods = [x[0] for x in food_data]

cost = dict([((x[0]), float(x[1])) for x in food_data])
cals = dict([((x[0]), float(x[3])) for x in food_data])
cholesterol = dict([((x[0]), float(x[4])) for x in food_data])
total_fat = dict([((x[0]), float(x[5])) for x in food_data])
sodium = dict([((x[0]), float(x[6])) for x in food_data])
carbs = dict([((x[0]), float(x[7])) for x in food_data])
fiber = dict([((x[0]), float(x[8])) for x in food_data])
protein = dict([((x[0]), float(x[9])) for x in food_data])
vit_a = dict([((x[0]), float(x[10])) for x in food_data])
vit_c = dict([((x[0]), float(x[11])) for x in food_data])
calcium = dict([((x[0]), float(x[12])) for x in food_data])
iron = dict([((x[0]), float(x[13])) for x in food_data])

problem = LpProblem("Diet Problem", LpMinimize)

amount_variables = LpVariable.dicts("Amounts", foods, 0)

problem += lpSum([cost[i] * amount_variables[i] for i in foods]), "total cost"

problem += lpSum([cals[i] * amount_variables[i] for i in foods]) >= 1500
problem += lpSum([cals[i] * amount_variables[i] for i in foods]) <= 2500

problem += lpSum([cholesterol[i] * amount_variables[i] for i in foods]) >= 30
problem += lpSum([cholesterol[i] * amount_variables[i] for i in foods]) <= 240

problem += lpSum([total_fat[i] * amount_variables[i] for i in foods]) >= 20
problem += lpSum([total_fat[i] * amount_variables[i] for i in foods]) <= 70

problem += lpSum([sodium[i] * amount_variables[i] for i in foods]) >= 800
problem += lpSum([sodium[i] * amount_variables[i] for i in foods]) <= 2000

problem += lpSum([carbs[i] * amount_variables[i] for i in foods]) >= 130
problem += lpSum([carbs[i] * amount_variables[i] for i in foods]) <= 450

problem += lpSum([fiber[i] * amount_variables[i] for i in foods]) >= 125
problem += lpSum([fiber[i] * amount_variables[i] for i in foods]) <= 250

problem += lpSum([protein[i] * amount_variables[i] for i in foods]) >= 60
problem += lpSum([protein[i] * amount_variables[i] for i in foods]) <= 100

problem += lpSum([vit_a[i] * amount_variables[i] for i in foods]) >= 1000
problem += lpSum([vit_a[i] * amount_variables[i] for i in foods]) <= 10000

problem += lpSum([vit_c[i] * amount_variables[i] for i in foods]) >= 400
problem += lpSum([vit_c[i] * amount_variables[i] for i in foods]) <= 5000

problem += lpSum([calcium[i] * amount_variables[i] for i in foods]) >= 700
problem += lpSum([calcium[i] * amount_variables[i] for i in foods]) <= 1500

problem += lpSum([iron[i] * amount_variables[i] for i in foods]) >= 10
problem += lpSum([iron[i] * amount_variables[i] for i in foods]) <= 40

binary_variables = LpVariable.dicts("InOrOut", foods, 0, 1, LpBinary)

print(amount_variables)
print(binary_variables)

# Works for 2.a
for i in foods:
    problem += (amount_variables[i] >= binary_variables[i] * 0.1)
    problem += (amount_variables[i] <= binary_variables[i] * 10000000000)

# 2.b
problem += (binary_variables["Celery, Raw"] + binary_variables["Frozen Broccoli"] <= 1)

# 2.c
problem += (binary_variables["Roasted Chicken"] + binary_variables["Poached Eggs"] +
            binary_variables["Scrambled Eggs"] + binary_variables["Bologna,Turkey"] +
            binary_variables["Frankfurter, Beef"] + binary_variables["Ham,Sliced,Extralean"] +
            binary_variables["Kielbasa,Prk"] + binary_variables["Taco"] +
            binary_variables["Hamburger W/Toppings"] + binary_variables["Hotdog, Plain"] +
            binary_variables["Pork"] + binary_variables["Sardines in Oil"] +
            binary_variables["White Tuna in Water"] + binary_variables["Chicknoodl Soup"] +
            binary_variables["Splt Pea&Hamsoup"] + binary_variables["Vegetbeef Soup"] +
            binary_variables["Neweng Clamchwd"] + binary_variables["New E Clamchwd,W/Mlk"] +
            binary_variables["Beanbacn Soup,W/Watr"] >= 3)

# Solve LP
problem.solve()

var_dictionary = {}
for v in problem.variables():
    var_dictionary[v.name] = v.varValue

    if v.varValue > 0 and "InOrOut" not in v.name:
        print(v.name, v.varValue)

print(var_dictionary)