clear all

%% liquid bridge
% basic parameters
liquid_tol = 48840;
liquid_dim = [0; 117.9; 86.46];
yatoms = 37;
zatoms = 3*11;
xatoms=liquid_tol/yatoms/zatoms;
liquid_latt = zeros(1,3);
liquid_latt(1) = 3.01;
liquid_latt(2) = liquid_dim(2)/(yatoms-1);
liquid_latt(3) = liquid_dim(3)/(zatoms-1);
liquid_dim(1) = liquid_latt(1)*xatoms;
liquid_start = [250; 0; 3*3.93];

% create and write positions
m=0;
nn=xatoms*yatoms*zatoms;
posx=zeros(nn,1);
posy=zeros(nn,1);
posz=zeros(nn,1);
for k=0:1:zatoms-1
    for j=0:1:yatoms-1
        for i=0:1:xatoms-1
            m=m+1;
            posx(m)=liquid_start(1) + i*liquid_latt(1);
            posy(m)=liquid_start(2) + j*liquid_latt(2);
            posz(m)=liquid_start(3) + k*liquid_latt(3);
        end
    end
end
% figure(618)
% plot3(posx,posy,posz,'.');
line2=zeros(nn,1)+1;
line3=zeros(nn,1)+1;

line1=1:1:nn;
lineall = [line1' line3 posx posy posz];
fileID = fopen('2atoms.dat','w');
% fprintf(fileID,'%i %i %i %f %f %f %i %i %i\n',lineall');
fprintf(fileID,'%i %i %f %f %f\n',lineall');
fclose(fileID);

%% Walls
wall_tol = 16275;
walls_tol = wall_tol*2;
latt = 3.93;
xatoms = 175;
yatoms = 31;
zatoms = 3;
bot_wall_start = [0; 0; 0];
top_wall_start = [0; 0; 102.18];

posx = zeros(walls_tol,1);
posy = zeros(walls_tol,1);
posz = zeros(walls_tol,1);
m=0;
for k=0:1:zatoms-1
    for j=0:1:yatoms-1
        for i=0:1:xatoms-1
            m=m+1;
            posx(m)=bot_wall_start(1) + i*latt;
            posy(m)=bot_wall_start(2) + j*latt;
            posz(m)=bot_wall_start(3) + k*latt;
        end
    end
end
for k=0:1:zatoms-1
    for j=0:1:yatoms-1
        for i=0:1:xatoms-1
            m=m+1;
            posx(m)=top_wall_start(1) + i*latt;
            posy(m)=top_wall_start(2) + j*latt;
            posz(m)=top_wall_start(3) + k*latt;
        end
    end
end
line1 = liquid_tol+1:1:walls_tol+liquid_tol;
line3 = zeros(walls_tol,1)+2;
lineall3 = [line1' line3 posx posy posz];
fileID = fopen('2atoms.dat','at');
fprintf(fileID,'%i %i %f %f %f\n',lineall3');
fclose(fileID);



