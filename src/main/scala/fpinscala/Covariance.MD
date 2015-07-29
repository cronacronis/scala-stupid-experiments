
#Covariant relation
We can assign ***smaller to bigger*** things.  

We cannot assign an Animal instance to a Tiger.
```C#
Mammal m = new Tiger();
Animal a = m;
//Tiger t = a;  -- will not compile
```

```C#
Mammal[] M = new Tiger[0];
Animal[] A = M;
//Tiger[] t = A;  -- will not compile
```


#Contravariant

 