#include<bits/stdc++.h> 
using namespace std;
#define int long long
#define Y cout<<"YES"<<endl
#define N cout<<"NO"<<endl
#define D cout<<"DEBUG"<<endl

void solve(){
    int n = 220*3;
    int j = -1;
    for(int i=0;i<n;i++){
        string s;
        cin>>s;
        // D;
        if(i%3 != 1)continue;
        // cout<<s<<endl;
        j++;
        if(s[0] == 'z'){
            cout<<"rg_man_output["<<j<<"] = readOnlyReg(0);"<<endl;
        }
        else{
            if(s[0] == '0' && s[1] == '0'){
                cout<<"rg_man_output["<<j<<"] = readOnlyReg("<<0<<");"<<endl;
            }
            else if(s[0] == '0' && s[1] == '1'){
                cout<<"rg_man_output["<<j<<"] = readOnlyReg(1);"<<endl;
            }
            else if(s[0] == '1' && s[1] == '0'){
                cout<<"rg_man_output["<<j<<"] = readOnlyReg(2);"<<endl;
            }
            else if(s[0] == '1' && s[1] == '1'){
                cout<<"rg_man_output["<<j<<"] = readOnlyReg(3);"<<endl;
            }
        }
    }
}
// 

int32_t main(){
    solve();
}

 
 
 
 
 
 




























