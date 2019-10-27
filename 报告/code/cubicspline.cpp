#include <iostream>
#include <math.h>
using namespace std;
#define max 50 

float x[max], y[max], h[max];//定义具体的数组来存放原始数据 
float c[max], a[max], fm[max]; 

float f(int x1, int x2, int x3) 
{ 
    float a = (y[x3] - y[x2]) / (x[x3] - x[x2]);   //追赶法 
    float b = (y[x2] - y[x1]) / (x[x2] - x[x1]); 
    return (a - b) / (x[x3] - x[x1]); 
} 

int main()     //求差分 
{ 
    void print(int n);    //函数声明 
    void chaf(int n);     //函数声明 
    int n, i; 
    char ch; 
    cout << "输入x的最大下标:"; 
    cin >> n; 
    for (i = 0; i <= n; i++) 
    { 
        cout <<"请输入x[" << i << "]="; 
        cin >> x[i]; 
    } 
    for (i = 0; i <= n; i++) 
    { 
        cout << "请输入y[" << i << "]="; 
        cin >> y[i]; 
    } 
    cout << endl; 
    do 
    { 
        for (i = 0; i < n; i++)            //求步长 
            h[i] = x[i + 1] - x[i]; 
        cout << "请输入边界条件:"; 
        int p; 
        float f0, f1; 
        cin >> p; 
        switch (p) 
        { 
        case 1: 
            cout << "输入Y0 Y" << n << "\n"; 
            cin >> f0 >> f1; 
            c[0] = 1; 
            a[n] = 1; 
            fm[0] = 6 * ((y[1] - y[0]) / (x[1] - x[0]) - f0) / h[0]; 
            fm[n] = 6 * (f1 - (y[n] - y[n - 1]) / (x[n] - x[n - 1])) / h[n - 1]; 
            break; 
        case 2: 
            cout << "输入Y0\" Y" << n << "\"\n"; 
            cin >> f0 >> f1; 
            c[0] = a[n] = 0; 
            fm[0] = 2 * f0; fm[n] = 2 * f1; 
            break; 
        default: 
            cout << "不可用\n";//待定 
        } 
        for (i = 1; i < n; i++) 
            fm[i] = 6 * f(i - 1, i, i + 1); 
        for (i = 1; i < n; i++) 
        { 
            a[i] = h[i - 1] / (h[i] + h[i - 1]); 
            c[i] = 1 - a[i]; 
        } 
        a[n] = h[n - 1] / (h[n - 1] + h[n]); 
        chaf(n);            //调用函数 
        cout << "插值函数："; 
        print(n);           //调用函数 
        cin >> ch; 
    }  
while (ch == 'y' || ch == 'Y'); 
system("pause"); 
return 0; 
} 

void print(int n) 
{ 
    cout << setprecision(6); 
    for (int i = 0; i < n; i++) 
    { 
        cout << i + 1 << ": (" << x[i] << " , " << x[i + 1] << ")\n" << "\t"; 
        cout << "S" << i + 1 << "="; 
        float t = fm[i] / (6 * h[i]); 
        if (t > 0) 
            cout << -t << "*(x-" << x[i + 1] << ")^3"; 
        else 
            cout << -t << "*(x-" << x[i + 1] << ")^3"; 
        t = fm[i + 1] / (6 * h[i]); 
        if (t > 0) 
            cout << " + " << t << "*(x-" << x[i] << ")^3"; 
        else 
            cout << "-" << t << "*(x-" << x[i] << ")^3"; 
        cout << "\n\t"; 
        t = (y[i] - fm[i] * h[i] * h[i] / 6) / h[i]; 
        if (t > 0) 
            cout << "- " << t << "*(x-" << x[i + 1] << ")"; 
        else 
            cout << "- " << -t << "*(x-" << x[i + 1] << ")"; 
        t = (y[i + 1] - fm[i + 1] * h[i] * h[i] / 6) / h[i]; 
        if (t > 0) 
            cout << " + " << t << "*(x-" << x[i] << ")"; 
        else 
            cout << "-" << -t << "*(x-" << x[i] << ")"; 
        cout << endl; 
    } 
    cout << endl; 
} 

void chaf(int n) 
{ 
    float B[max]; 
    B[0] = c[0] / 2; 
    int i; 
    for (int i = 1; i < n; i++) 
        B[i] = c[i] / (2 - a[i] * B[i - 1]); 
    fm[0] = fm[0] / 2; 
    for (i = 1; i <= n; i++) 
        fm[i] = (fm[i] - a[i] * fm[i - 1]) / (2 - a[i] * B[i - 1]); 
    for (i = n - 1; i >= 0; i--) 
        fm[i] = fm[i] - B[i] * fm[i + 1]; 
}
