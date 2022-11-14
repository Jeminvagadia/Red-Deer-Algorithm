#include <bits/stdc++.h>
#include <limits.h>
#include <omp.h>
using namespace std;

#define V 25
#define TOT_GEN 100
#define ALPHA 0.2
#define BETA 0.1
#define GAMMA 0.2
#define POP_SIZE 200
#define ELITIST 0.2
#define UB 1
#define LB 0
#define INF 9999.00
#define nthreads 4

float matrix[V][V];

struct individual {
	vector<float> gnome;
	float fitness;
};

void disp(vector<float> gnome){
	vector<pair<float,int> > temp;
	for(int i=0;i<gnome.size();i++){
		temp.push_back(make_pair(gnome[i],i));
	}
	sort(temp.begin(), temp.end());
	for(int i=0;i<temp.size();i++){
		cout<<temp[i].second;
	}
}

float cal_fitness(vector<float> gnome)   
{
	vector<pair<float,int> > temp;
	for(int i=0;i<V;i++){
		temp.push_back(make_pair(gnome[i],i));
	}
	sort(temp.begin(), temp.end());

	float f = 0;
	for (int i = 0; i < V - 1; i++) {
		if (matrix[temp[i].second][temp[i+1].second] == INF)
			return 0;
		f += matrix[temp[i].second][temp[i+1].second];
	}
	if(matrix[temp[V-1].second][temp[0].second]==INF){
		return 0;
	}
	f+=matrix[temp[V-1].second][temp[0].second];
	float f1= 1/f;
	return f1;
}

vector<float> create_gnome()
{
	vector<float> gnome;
	for(int i=0;i<V;i++){
		float r=((float) rand() / (RAND_MAX));
		gnome.push_back(r);
	}
	return gnome;
}

bool compare(struct individual t1,
			struct individual t2)
{
	return t1.fitness > t2.fitness;
}

vector<struct individual> roar(vector<struct individual> male){
    for(int i=0;i<male.size();i++){
        struct individual temp;
        for(int j=0;j<V;j++){
            float a1=((float) rand() / (RAND_MAX));
            float a2=((float) rand() / (RAND_MAX));
            float a3=((float) rand() / (RAND_MAX));
            if(a3>=0.5){
                temp.gnome.push_back(male[i].gnome[j] + (a1*( ( (UB-LB)*a2 ) + LB)));
            }
            else{
                temp.gnome.push_back(male[i].gnome[j] - (a1*( ( (UB-LB)*a2 ) + LB)));
            }
        }
        temp.fitness = cal_fitness(temp.gnome);
        if(temp.fitness > male[i].fitness){
            male[i]=temp;
        }
    }
    return male;
}

struct individual fight(struct individual comm ,struct individual stag){
	struct individual new1;
	struct individual new2;
	for(int i=0;i<V;i++){
		float b1=((float) rand() / (RAND_MAX));
		float b2=((float) rand() / (RAND_MAX));
		float temp1,temp2;
		temp1= (comm.gnome[i]+stag.gnome[i])/2 + (b1*( ( (UB-LB)*b2 ) + LB));
		temp2= (comm.gnome[i]+stag.gnome[i])/2 - (b1*( ( (UB-LB)*b2 ) + LB));
		new1.gnome.push_back(temp1);
		new2.gnome.push_back(temp2);
	}
	new1.fitness=cal_fitness(new1.gnome);
	new2.fitness=cal_fitness(new2.gnome);
	float max1=max(max(comm.fitness,stag.fitness),max(new1.fitness,new2.fitness));
	if(comm.fitness==max1){
		return comm;
	}
	else if(stag.fitness==max1){
		return stag;
	}
	else if(new1.fitness==max1){
		return new1;
	}
	else{
		return new2;
	}
}

vector<struct individual> fighting(vector<struct individual> male){
    int Ncomm = male.size()*GAMMA;
    vector<struct individual> commanders;
    vector<struct individual> stags;

    for(int i=0;i<Ncomm;i++){
        commanders.push_back(male[i]);
    }

    for(int i=Ncomm;i<male.size();i++){
        stags.push_back(male[i]);
    }

    for(int i=0;i<commanders.size();i++){
        int r= rand()%stags.size();
        commanders[i]=fight(commanders[i],stags[r]);
    }

    vector<struct individual> new_male;

    new_male.insert(new_male.end(), commanders.begin(), commanders.end());
    new_male.insert(new_male.end(), stags.begin(), stags.end());
    return new_male;
}

struct individual mate(struct individual p1,struct individual p2){
	struct individual children;
	for(int i=0;i<V;i++){
		float c=((float) rand() / (RAND_MAX));
		float temp;
		temp= (p1.gnome[i]+p2.gnome[i])/2 + ((UB-LB)*c);
		children.gnome.push_back(temp);
	}
	children.fitness=cal_fitness(children.gnome);
	return children;		
}

vector<struct individual> alphamating(struct individual male, vector<struct individual> harem){
	random_shuffle(harem.begin(), harem.end());
	int size=harem.size()*ALPHA;
	vector<struct individual> child;
	for(int i=0;i<size;i++){
		child.push_back(mate(male,harem[i]));
	}
	return child;
}

vector<struct individual> betamating(struct individual male, vector<struct individual> harem){
	random_shuffle(harem.begin(), harem.end());
	int size=harem.size()*BETA;
	vector<struct individual> child;
	for(int i=0;i<size;i++){
		child.push_back(mate(male,harem[i]));
	}
	return child;
}

struct individual nearest(struct individual stag,vector<struct individual> hinds){
	int index;
	float min=99999;
	for(int i=0;i<hinds.size();i++){
		float distance=0;
		for(int j=0;j<V;j++){
			distance+= pow(stag.gnome[j]-hinds[i].gnome[j],2);
		}
		distance=sqrt(distance);
		if(distance<min){
			index=i;
			min=distance;
		}
	}
	return hinds[index];
}

vector<struct individual> mating(vector<struct individual> male, vector<struct individual> hinds){
	int Ncomm = male.size()*GAMMA;
	random_shuffle(hinds.begin(), hinds.end());
	vector<struct individual> harems[Ncomm];
	vector<struct individual> offsprings;
	float tot_fitness=0;
	for(int i=0;i<Ncomm;i++){
		tot_fitness+= male[i].fitness;
	}
	int k=0;
	for(int i=0;i<Ncomm;i++){
		int size= (male[i].fitness/tot_fitness) * hinds.size();
		int itr=k;
		k=k+size;
		while(itr<k){
			harems[i].push_back(hinds[itr]);
			itr++;
		}
	}

	if(Ncomm>0){
	    int j=0;
	    while(k < hinds.size()){
		    harems[j].push_back(hinds[k]);
		    k++;
		    j=(j+1)%Ncomm;
	    }
	}

	for(int i=0;i<Ncomm;i++){
		vector<struct individual> child1,child2;
		child1=alphamating(male[i],harems[i]);
		int r=rand()%Ncomm;
		child2=betamating(male[i],harems[r]);
		offsprings.insert(offsprings.end(), child1.begin(), child1.end());
		offsprings.insert(offsprings.end(), child2.begin(), child2.end());
	}

	for(int i=Ncomm;i<male.size();i++){
		struct individual child;
		struct individual nearest_hind;
		nearest_hind=nearest(male[i],hinds);
		child=mate(male[i],nearest_hind);
		offsprings.push_back(child);		
	}
	return offsprings;
}

vector<struct individual> roulette(vector<struct individual> tot_hinds, int size){
	vector<struct individual> rest_of_population;
	float tot=0;
	for(int i=0;i<tot_hinds.size();i++){
		tot+=tot_hinds[i].fitness;
	}

	for(int j=0;j<size;j++){		
		float rd=((float) rand() / (RAND_MAX));
		float sum=0;
		for(int k=0;k<tot_hinds.size();k++){
			sum+=(tot_hinds[k].fitness)/tot;
			if(sum>rd){
				rest_of_population.push_back(tot_hinds[k]);
				break;
			}
		}
	}
	return rest_of_population;
}

int main()
{
	srand(time(0));
	
	freopen("input_rda.txt", "r", stdin);
	for(int i=0;i<V;i++){
	    for(int j=0;j<V;j++){
	        cin>>matrix[i][j];
	    }
	}
	fclose (stdin);
	
	int gen = 1;
	
	omp_set_num_threads(nthreads);

	vector<struct individual> population;
	struct individual temp;

	for (int i = 0; i < POP_SIZE; i++) {
		temp.gnome = create_gnome();
		temp.fitness = cal_fitness(temp.gnome);
		population.push_back(temp);
	}

	int best=0;
	for(int i=0;i<population.size();i++){
		if(population[i].fitness>population[best].fitness){
			best = i;
		}
	}
	cout<<"Best fitness value in initial population is: "<<population[best].fitness<<endl<<endl;

	double begin = omp_get_wtime();
	while ( gen <= TOT_GEN) {
	     vector<struct individual>new_population;
	     #pragma omp parallel
	     {
		int id= omp_get_thread_num();
		int size=(population.size()/nthreads);
	           vector<struct individual>population1;
	           
	           for(int i=id*size;i<(id+1)*size;i++){
	               population1.push_back(population[i]);
	           }
	           if(id==nthreads-1){
	               for(int i=(id+1)*size;i<population.size();i++){
	                   population1.push_back(population[i]);
	               }
	           }
	        sort(population1.begin(), population1.end(), compare);
		vector<struct individual> new_population1;
		vector<struct individual> male;
		vector<struct individual> hinds;
		vector<struct individual> offspring;
		int Nmale=population1.size()*ELITIST;
		for(int i=0;i<Nmale;i++){
		    male.push_back(population1[i]);
		}
		for(int i=Nmale;i<population1.size();i++){
		    hinds.push_back(population1[i]);
		}

		male=roar(male);
		random_shuffle(male.begin(), male.end());
		male=fighting(male);
		offspring=mating(male,hinds);
		
		vector<struct individual> tot_hinds;
		
		tot_hinds.insert(tot_hinds.end(), offspring.begin(), offspring.end());
		tot_hinds.insert(tot_hinds.end(), hinds.begin(), hinds.end());
		
		sort(tot_hinds.begin(), tot_hinds.end(), compare);
		
		new_population1=roulette(tot_hinds,population1.size()-male.size());
		
		new_population1.insert(new_population1.end(), male.begin(), male.end());

		population1 = new_population1;
		
		#pragma omp critical
		   {  
		       new_population.insert(new_population.end(), new_population1.begin(), new_population1.end());
		   }
	       }
	      
	      random_shuffle(new_population.begin(), new_population.end());
	      population=new_population;	      
	    
		if(gen%10==0 || gen==TOT_GEN){	
		 int best1=0;
	          for(int i=0;i<population.size();i++){
		    if(population[i].fitness>population[best1].fitness){
			    best1 = i;
		    }
	          }
	         cout<<"Best fitness value in gen "<<gen<<" is: "<<population[best1].fitness<<endl<<endl;	
	        }	
	      gen++;
	}
	double end =omp_get_wtime();
	double time_taken=(end-begin);
	cout<<"\n\nFinished solving "<<endl;
	cout<<"Time taken by "<<nthreads<<" threads in execution is : "<<fixed << time_taken <<setprecision(5);
	cout<<" seconds "<<endl;
}
