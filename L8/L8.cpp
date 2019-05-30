#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <algorithm>
#include <queue>
#include <functional>
#include <map>
using namespace std;

#define EPS 0.01

//Метод минимального элемента
void method_min_elem(map<int, vector<pair<int, int>>> &c1, const vector<int> &N, const vector<int> &M, vector<vector<int>> &x) {
	vector<int> N_ = N, M_ = M;
	for (auto t : c1) {
		while (true) {
			int maxVal = 0, iMax, jMax;
			for (auto pr : t.second) {
				int iCur = pr.first, jCur = pr.second;
				int curVal = min(N_[iCur], M_[jCur]);
				if (curVal >= maxVal) {
					maxVal = curVal;
					iMax = iCur, jMax = jCur;
				}
			}
			if (maxVal != 0) {
				N_[iMax] -= maxVal;
				M_[jMax] -= maxVal;
				x[iMax][jMax] = maxVal;
			}
			else
				break;
		}
	}
}


//Метод северо-западного угла
int method_nw_angle(const vector<double> &N, const vector<double> &M, vector<vector<double>> &x) {
	vector<double> N_ = N, M_ = M;
	int k = 0, maxK = N.size() + M.size() - 1, count = 0;
	int curI = 0, curJ = 0;
	while (k < maxK) {
		double val = min(N_[curI], M_[curJ]);
		x[curI][curJ] = val;
		count++;
		N_[curI] -= val;
		int f = 0;
		if (N_[curI] == 0) {
			curI++;		//Вычеркнули строку curI
			k++;
			f++;
		}
		M_[curJ] -= val;
		if (M_[curJ] == 0) {
			curJ++;		//Вычеркнули столбец curJ
			k++;
			f++;
		}
		if (curI - 1 >= 0 && curJ - 1 >= 0 && curJ < M.size() && f == 2)
			x[curI - 1][curJ] = EPS;		//Вырожденный случай
	}
	return count;
}

void print_res(vector<vector<double>> &x, vector<vector<int>> &c, ofstream &fout) {
	fout << "\nТаблица поставок. Объём (цена)\n" << endl;
	int n = x.size(), m = x[0].size();
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < m; j++)
			if (x[i][j] == -1)
				fout << "-\t\t";
			else {
				//int xCur = (int)round((x[i][j] / KOEF) / 10.0) * 10;
				int xCur = (int)round(x[i][j]);
				if (xCur != 0)
					fout << xCur << "(" << c[i][j] << ")\t";
				else
					fout << "-\t\t";

			}
		fout << "\n";
	}

	int sum = 0;
	for (int i = 0; i < n; i++) {
		for (int j = 0; j < m; j++)
			if (x[i][j] != -1) {
				//fout << "Поставщик " << i + 1 << " отправляет " << x[i][j] << "ед. товара потребителю " << j + 1 << " c расходами в размере " << x[i][j] * c[i][j] << " руб.\n";
				sum += (int)round(x[i][j]) * c[i][j];
			}
	}
	fout << "\nОбщая сумма расходов = " << sum << endl;
	fout.flush();
}


bool dfs(int curI, int curJ, vector<vector<double>> &x, vector<vector<bool>> &used, vector<pair<int, int>> &path, int count, int startI, int startJ, bool isRow) {
	used[curI][curJ] = true;
	path.push_back({ curI, curJ });

	if (isRow) {
		//Обход по строке
		for (int j = 0; j < x[curI].size(); j++) {
			if (x[curI][j] != -1 && !used[curI][j]) {
				if (dfs(curI, j, x, used, path, count + 1, startI, startJ, false))
					return true;
			}
			else if (j == startJ && curI == startI && count > 2 && count % 2 == 0) {		//Замкнутый маршрут с четным числом вершин найден
				return true;
			}
		}
	}
	else {
		//Обход по столбцу
		for (int i = 0; i < x.size(); i++) {
			if (x[i][curJ] != -1 && !used[i][curJ]) {
				if (dfs(i, curJ, x, used, path, count + 1, startI, startJ, true))
					return true;
			}
			else if (i == startI && curJ == startJ && count > 2 && count % 2 == 0) {	//Замкнутый маршрут с четным числом вершин найден
				return true;
			}
		}
	}

	path.pop_back();
	used[curI][curJ] = false;
}


void find_potential(vector<int> &pN, vector<int> &pM, vector<bool> &pN_, vector<bool> &pM_, vector<vector<double>> &x, vector<vector<int>> &c) {
	vector<int> columns, rows;
	rows.push_back(0);
	pN_[0] = true;
	int countBase = pM.size() + pN.size() - 1;

	for (int k = 0; k < countBase;) {

		int count = 0;
		for (int ii : rows) {		//Обход по строкам из rows
			for (int j = 0; j < pM.size(); j++)		//Обход по строке
				if (x[ii][j] != -1 && !pM_[j]) {
					pM[j] = c[ii][j] + pN[ii];
					pM_[j] = true;
					k++;
					count++;
					columns.push_back(j);
				}
		}
		if (count)
			rows.clear();
		else {			//Вырожденный случай
			int j, ii = rows[0];
			for (j = 0; j < pM.size(); j++)
				if (!pM_[j])
					break;
			x[ii][j] = EPS;
			pM[j] = c[ii][j] + pN[ii];
			pM_[j] = true; 
			k++;
			columns.push_back(j);
		}
		if (k == countBase)
			break;

		count = 0;
		for (int jj : columns) {		//Обход по строкам из columns
			for (int i = 0; i < pN.size(); i++)		//Обход по столбцу
				if (x[i][jj] != -1 && !pN_[i]) {
					pN[i] = pM[jj] - c[i][jj];
					pN_[i] = true;
					k++;
					count++;
					rows.push_back(i);
				}
		}
		if (count)
			columns.clear();
		else {			//Вырожденный случай
			int i, jj = columns[0];
			for (i = 0; i < pN.size(); i++)
				if (!pN_[i])
					break;
			x[i][jj] = EPS;
			pN[i] = pM[jj] - c[i][jj];
			pN_[i] = true;
			k++;
			rows.push_back(i);
		}
	}
}


int main()
{
	ifstream fin("inp.txt");
	ofstream fout("out.txt");

	int T, n, m;
	fin >> T;
	for (int t = 1; t <= T; t++) {
		fout << "\n\n\n\t\tТест " << t << endl;
		fin >> n >> m;
		vector<double> N(n), M(m);
		int sumN = 0, sumM = 0;
		for (int i = 0; i < n; i++) {
			fin >> N[i];
			sumN += N[i];
		}
		for (int i = 0; i < m; i++) {
			fin >> M[i];
			sumM += M[i];
		}

		vector<vector<int>> c(n, vector<int>(m));
		vector<vector<double>>x (n, vector<double>(m, -1));
		map<int, vector<pair<int, int>>> c1;


		for (int i = 0; i < n; i++)
			for (int j = 0; j < m; j++) {
				fin >> c[i][j];
				c1[c[i][j]].push_back({ i, j });
			}

		double deltaSum = sumN - sumM;		//Условие сбалансированности
		if (deltaSum > 0) {				//Не хватает потребителей
			m++;
			M.push_back(deltaSum);
			for (int i = 0; i < n; i++) {
				c[i].push_back(0);
				x[i].push_back(-1);
			}
		}
		else if (deltaSum < 0) {		//Не хватает поставщиков
			n++;
			N.push_back(-deltaSum);
			x.push_back(vector<double>(m, -1));
			c.push_back(vector<int>(m, 0));
		}

		int countBase = method_nw_angle(N, M, x);
		//method_min_elem(c1, N, M, x_min);
		for (int i = 0; i < n; i++)
			if (deltaSum < 0 && i == n - 1)
				fout << "Запас груза у поставщика " << i + 1 << " = " << N[i] << " (фиктивный) " << endl;
			else
				fout << "Запас груза у поставщика " << i + 1 << " = " << N[i] << endl;
		for (int i = 0; i < m; i++)
			if (deltaSum > 0 && i == m - 1)
				fout << "Спрос у потребителя  " << i + 1 << " = " << M[i] << " (фиктивный) " << endl;
			else
				fout << "Спрос у потребителя  " << i + 1 << " = " << M[i] << endl;

		fout << "\n\n\n\t\tНачальный опорный план перевозок:\n";
		print_res(x, c, fout);
		

		int countIter = 0;
		while (true) {

				//Поиск потенциалов
			vector<int> pN(n, 0), pM(m, 0);
			vector<bool> pN_(n, false), pM_(m, false);
			find_potential(pN, pM, pN_, pM_, x, c);

			/*fout << "\nIter " << countIter << "\n";
			print_res(x, c, fout);*/

				//Вспомогательная матрица h
			int hMin = 0, iMin = -1, jMin = -1, h = 0;
			for (int i = 0; i < n; i++)
				for (int j = 0; j < m; j++) {
					h = c[i][j] - (pM[j] - pN[i]);
					if (h < hMin) {
						hMin = h;
						iMin = i;
						jMin = j;
					}
				}


			if (hMin >= 0) {		//Оптимальный план найден
				break;
			}
			else {
					//Ищем замкнутый контур в X
				vector<vector<bool>> used(n, vector<bool>(m, false));
				vector<pair<int, int>> path;
				dfs(iMin, jMin, x, used, path, 1, iMin, jMin, true);


					//Поиск минимального четного в пути Х
				double minX = (double)INT_MAX;
				int k = 1;
				for (auto pr : path) {
					if (x[pr.first][pr.second] != -1 && k % 2 == 0)
						minX = min(x[pr.first][pr.second], minX);
					k++;
				}

					//Модификация X
				k = 1;
				for (auto pr : path) {
					if (k % 2) {
						if (x[pr.first][pr.second] == -1)
							x[pr.first][pr.second]++;
						x[pr.first][pr.second] += minX;
						if (x[pr.first][pr.second] == 0)
							x[pr.first][pr.second] = -1;
					}
					else {
						if (x[pr.first][pr.second] == -1)
							x[pr.first][pr.second]++;
						x[pr.first][pr.second] -= minX;
						if (x[pr.first][pr.second] == 0)
							x[pr.first][pr.second] = -1;
					}
					k++;
				}
			}
			countIter++;
		}


		//Вывод	

		fout << "\n\n\n\t\tИтоговый план перевозок\n";
		print_res(x, c, fout);
		fout << "Количество итераций = " << countIter << endl;
	}


	return 0;
}

