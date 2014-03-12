#include <algorithm>
#include <bitset>
#include <cctype>
#include <cerrno>
#include <clocale>
#include <cmath>
#include <complex>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <ctime>
#include <cwchar>
#include <cwctype>
#include <deque>
#include <exception>
#include <fstream>
#include <functional>
#include <iomanip>
#include <ios>
#include <iosfwd>
#include <iostream>
#include <istream>
#include <ostream>
#include <limits>
#include <list>
#include <map>
#include <numeric>
#include <queue>
#include <set>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <streambuf>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

using namespace std;

#define mp make_pair
#define pb push_back
#define rep(i,m,n) for(long long i = m; i < n; ++i)
#define re return
#define fi first
#define se second
#define sz(x) ((int) (x).size())
#define all(x) (x).begin(), (x).end()
#define sqr(x) ((x) * (x))
#define eps 1e-7
#define FOR(it,c) for(typeof((c).begin()) it=(c).begin();it!=(c).end();++it)

typedef vector<int> vi;
typedef vector<double> vd;
typedef vector<vd> vvd;
typedef vector<vvd> vvvd;
typedef long long ll;
typedef long double ld;
typedef pair<int, int> ii;
typedef vector<ii> vii;
typedef vector<string> vs;
typedef vector<ll> vll;
typedef vector<vi> vvi;
typedef vector<vll> vvll;
typedef vector<vii> vvii;

const double init_cap=100, buyfee=0.0005, sellfee=0.001, INF=1000000000.0;
//[init_week, final_week); S= the number of stocks +1;
const int r0=1314807, c0=13, upper_limit=64, init_week=764, final_week=1153, week_span=52, ipo_limit=55, change_span=26, W=1154, S=2662;
/*
 * series[][][?]:
 * [0]: 0 means pre-IPO or delisted; 1 means trading week; 2 means suspension period.
 * [1]: Weekly opening price.
 * [2]: Weekly highest price.
 * [3]: Weekly lowest price.
 * [4]: Weekly closing price.
 * [5]: Highest price of the first day of the week.
 * [6]: lowest price of the first day of the week.
 * [7]: Transaction status. 0: suspension period; 1: normal; 2: ST; 3: *ST; 4: S; 5: SST; 6: S*ST.
 */
vvvd series(W,vvd(S,vd(8,0)));
class Position{
 public:
  int symbol;
  double value;
  double change=0;
  int status=0;
};
bool IsAcceptableStatus (int week, int sym){
  re (series[week][sym][7]==1||series[week][sym][7]==2||series[week][sym][7]==4||series[week][sym][7]==5)&&week-ipo_limit>=0&&series[week-ipo_limit][sym][0];
}
struct ChangeComp {
  bool operator() (const Position& lhs, const Position& rhs) const{
    if(lhs.change>rhs.change){
      re true;
    }else if(lhs.change==rhs.change){
      re lhs.symbol<rhs.symbol;
    }else{
      re false;
    }
  }
} myChangeComp;
struct ChangeComp2 {
  bool operator() (const Position& lhs, const Position& rhs) const{
    if(lhs.change<rhs.change){
      re true;
    }else if(lhs.change==rhs.change){
      re lhs.symbol<rhs.symbol;
    }else{
      re false;
    }
  }
} myChangeComp2;
bool CanBeBought(int week, int sym){
  re series[week][sym][0]==1&&series[week-1][sym][0]==1&&!(series[week][sym][1]>series[week-1][sym][4]*1.095&&series[week][sym][5]==series[week][sym][6]);
}
bool CanBeSold(int week, int sym){
  re series[week][sym][0]==1&&series[week-1][sym][0]==1&&!(series[week][sym][1]<series[week-1][sym][4]*0.905&&series[week][sym][5]==series[week][sym][6]);
}
bool CanBeTransacted(int week, int sym){
  re CanBeBought(week, sym)&&CanBeSold(week, sym);
}
bool HitAHigh(int week, int sym){
  if(!(week-week_span>=0&&series[week-week_span][sym][0])) re false;
  double highest_price=0;
  bool new_high=true;
  rep(i,week-week_span,week)
    highest_price=max(highest_price,series[i][sym][4]);
  rep(i,week-week_span/2,week)
    if(series[i][sym][4]==highest_price)
      new_high=false;
  //re series[week][sym][4]>highest_price;
  re series[week][sym][4]>highest_price&&new_high;
}
bool HitALow(int week, int sym){
  if(!(week-week_span>=0&&series[week-week_span][sym][0])) re false;
  double lowest_price=INF;
  rep(i,week-week_span,week)
    lowest_price=min(lowest_price,series[i][sym][4]);
  re series[week][sym][4]<lowest_price;
}
double GetNetValue(vector<vector<Position>> &portfolios,int a){
  double res=0;
  rep(i,0,sz(portfolios[a]))
    res+=portfolios[a][i].value;
  re res;
}
vector<Position> Preparation1(int week, int &cnts, int &cntb, vector<vector<Position>> &portfolios){
  int zzz=week-init_week+1, n=sz(portfolios[zzz-1]), not_to_be_sold=0, to_be_sold=0;
  //Stocks to buy;
  vector<Position> candidate;
  rep(i,1,S){
    if(IsAcceptableStatus (week, i)&&CanBeTransacted(week, i)&&HitAHigh(week-1, i)){
      Position tem;
      tem.symbol=i;
      tem.change=series[week-1][i][4]/series[week-1-change_span][i][4];
      tem.status=1;
      candidate.pb(tem);
    }
  }
  int nc=sz(candidate);
  sort(candidate.begin(),candidate.end(),myChangeComp);
  //Stocks to sold;
  rep(i,1,n){
    portfolios[zzz-1][i].change=series[week-1][portfolios[zzz-1][i].symbol][4]/series[week-1-change_span][portfolios[zzz-1][i].symbol][4];
    if(!CanBeTransacted(week, portfolios[zzz-1][i].symbol)&&series[week][portfolios[zzz-1][i].symbol][4]){
      portfolios[zzz-1][i].status=-1;
      ++not_to_be_sold;
      rep(j,0,nc)
        if(portfolios[zzz-1][i].symbol==candidate[j].symbol){
          candidate[j].status=0;
          break;
        }
    }else if(!IsAcceptableStatus (week-1, portfolios[zzz-1][i].symbol)||HitALow(week-1, portfolios[zzz-1][i].symbol)||series[week][portfolios[zzz-1][i].symbol][4]==0){
      portfolios[zzz-1][i].status=1;
      ++to_be_sold;
    }else{
      portfolios[zzz-1][i].status=0;
    }
  }
  sort(portfolios[zzz-1].begin()+1,portfolios[zzz-1].end(),myChangeComp2);
  cntb=upper_limit-not_to_be_sold;
  rep(i,0,nc){
    if(cntb&&candidate[i].status==1){
      --cntb;
    }else{
      candidate[i].status=0;
    }
  }
  cntb=upper_limit-not_to_be_sold-cntb;
  cnts=max(0,cntb-(to_be_sold+upper_limit+1-n));
  rep(i,1,n){
    if(cnts&&portfolios[zzz-1][i].status==0){
      --cnts;
      portfolios[zzz-1][i].status=1;
    }else if(portfolios[zzz-1][i].status==0){
      portfolios[zzz-1][i].status=-1;
    }
  }
  cnts=to_be_sold+max(0,cntb-(to_be_sold+upper_limit+1-n))-cnts;
  rep(i,1,n)
    if(portfolios[zzz-1][i].status==1)
      rep(j,0,nc)
        if(candidate[j].status==1&&portfolios[zzz-1][i].symbol==candidate[j].symbol){
          --cnts;
          --cntb;
          candidate[j].status=0;
          portfolios[zzz-1][i].status=-1;
          break;
        }
  re candidate;
}
vector<Position> Preparation2(int week, int &cnts, int &cntb, vector<vector<Position>> &portfolios){
  int zzz=week-init_week+1, n=sz(portfolios[zzz-1]), cnt=upper_limit;
  //Stocks to buy;
  vector<Position> candidate;
  rep(i,1,S){
    if(IsAcceptableStatus (week, i)&&CanBeTransacted(week, i)&&HitAHigh(week-1, i)){
      Position tem;
      tem.symbol=i;
      tem.change=series[week-1][i][4]/series[week-1-change_span][i][4];
      tem.status=0;
      candidate.pb(tem);
    }
  }
  int nc=sz(candidate);
  sort(candidate.begin(),candidate.end(),myChangeComp);
  //Stocks to sold;
  rep(i,1,n){
    portfolios[zzz-1][i].change=series[week-1][portfolios[zzz-1][i].symbol][4]/series[week-1-change_span][portfolios[zzz-1][i].symbol][4];
    if(!CanBeTransacted(week, portfolios[zzz-1][i].symbol)&&series[week][portfolios[zzz-1][i].symbol][4]){
      portfolios[zzz-1][i].status=-1;
      --cnt;
      rep(j,0,nc)
        if(portfolios[zzz-1][i].symbol==candidate[j].symbol){
          candidate[j].status=0;
          break;
        }
    }else if(!IsAcceptableStatus (week, portfolios[zzz-1][i].symbol)||HitALow(week-1, portfolios[zzz-1][i].symbol)||series[week][portfolios[zzz-1][i].symbol][4]==0){
      portfolios[zzz-1][i].status=1;
    }else{
      portfolios[zzz-1][i].status=0;
    }
  }
  sort(portfolios[zzz-1].begin()+1,portfolios[zzz-1].end(),myChangeComp);
  int a=1, b=0;
  while(a<n||b<nc){
    if(a<n&&b<nc){
      if(cnt){
        if(portfolios[zzz-1][a].change<candidate[b].change){
          --cnt;
          candidate[b].status=1;
          ++b;
        }else if(portfolios[zzz-1][a].change==candidate[b].change){
          if(portfolios[zzz-1][a].symbol==candidate[b].symbol){
            if(portfolios[zzz-1][a].status==0){
              portfolios[zzz-1][a].status=-1;
              --cnt;
            }
            ++a;
            ++b;
          }else if(portfolios[zzz-1][a].symbol<candidate[b].symbol){
            if(portfolios[zzz-1][a].status==0){
              portfolios[zzz-1][a].status=-1;
              --cnt;
            }
            ++a;
          }else{
            candidate[b].status=1;
            ++b;
            --cnt;
          }
        }else{
          if(portfolios[zzz-1][a].status==0){
            portfolios[zzz-1][a].status=-1;
            --cnt;
          }
          ++a;
        }
      }else{
        while(a<n){
          portfolios[zzz-1][a].status=(portfolios[zzz-1][a].status==-1?-1:1);
          ++a;
        }
      }
    }else if(!cnt){
      while(a<n){
        portfolios[zzz-1][a].status=(portfolios[zzz-1][a].status==-1?-1:1);
        ++a;
      }
      break;
    }else{
      while(cnt){
        if(a<n){
          if(portfolios[zzz-1][a].status==0){
            portfolios[zzz-1][a].status=-1;
            --cnt;
          }
          ++a;
        }else if(b<nc){
          candidate[b].status=1;
          --cnt;
          ++b;
        }else{
          break;
        }
      }
    }
  }
  cnts=0;
  cntb=0;
  rep(i,1,n) if(portfolios[zzz-1][i].status==1) ++cnts;
  rep(i,0,nc) if(candidate[i].status==1) ++cntb;
  re candidate;
}
string PrintSymbol(int sym){
  if(!sym) re "0";
  int a=log10(sym)+1;
  stringstream ss;
  string res;
  rep(i,a,6) ss<<'0';
  ss<<sym;
  ss>>res;
  re res;
}
int main() {
  ios_base::sync_with_stdio(false);
  ifstream fin("data2333.in");
  ifstream fname("nametable.in");
  ofstream fout("submission.out");
  int n_name_table;
  fname>>n_name_table;
  vi name_table(n_name_table);
  rep(i,0,n_name_table) fname>>name_table[i];
  //Import the data.
  rep(i,0,W) rep(j,0,S) rep(k,0,8) fin>>series[i][j][k];
  //Now start to code the strategy
  vd net_worth(final_week-init_week+1,0);
  net_worth[0]=init_cap;
  vector<vector<Position>> portfolios(final_week-init_week+1,vector<Position>(1));
  portfolios[0][0].symbol=0, portfolios[0][0].value=init_cap;
  rep(week,init_week,final_week){
    int zzz=week-init_week+1, cnts, cntb;
    vector<Position> candidate=Preparation1(week, cnts, cntb, portfolios);
    //vector<Position> candidate=Preparation2(week, cnts, cntb, portfolios);
    //rep(i,0,sz(candidate)) fout<<PrintSymbol(name_table[candidate[i].symbol])<<' ';
    //fout<<endl;
    //fout<<cntb<<endl;
    int n=sz(portfolios[zzz-1]), nc=sz(candidate);
    portfolios[zzz][0].symbol=0, portfolios[zzz][0].value=portfolios[zzz-1][0].value;
    //sell
    rep(i,1,n){
      if(portfolios[zzz-1][i].status==-1){
        Position tem;
        tem.symbol=portfolios[zzz-1][i].symbol;
        tem.value=portfolios[zzz-1][i].value*series[week][tem.symbol][4]/series[week-1][tem.symbol][4];
        portfolios[zzz].pb(tem);
      }else if(portfolios[zzz-1][i].status==1){
        if(series[week][portfolios[zzz-1][i].symbol][1])
          portfolios[zzz][0].value+=portfolios[zzz-1][i].value*series[week][portfolios[zzz-1][i].symbol][1]/series[week-1][portfolios[zzz-1][i].symbol][4]*(1-sellfee);
        else
          portfolios[zzz][0].value+=portfolios[zzz-1][i].value*(1-sellfee);
      }
    }
    //buy
    double value_to_buy=portfolios[zzz][0].value/(upper_limit+1-n+cnts);
    portfolios[zzz][0].value=max(0.0,portfolios[zzz][0].value-value_to_buy*cntb);
    rep(i,0,nc)
      if(candidate[i].status==1){
        Position tem;
        tem.symbol=candidate[i].symbol;
        tem.value=value_to_buy*series[week][tem.symbol][4]/series[week][tem.symbol][1]*(1-buyfee);
        portfolios[zzz].pb(tem);
      }
    //calculate the net value;
    net_worth[zzz]=GetNetValue(portfolios, zzz);
    //fout<<week<<' '<<sz(portfolios[zzz])-1<<' ';
    //rep(i,0,sz(portfolios[zzz])){fout<<PrintSymbol(name_table[portfolios[zzz][i].symbol])<<' ';} fout<<endl;
  }
  //output the results
  rep(i,0,sz(net_worth)){fout<<net_worth[i]<<endl;}
  return 0;
}
