clear all

%% liquid bridge
% basic parameters
liquid_tol = 48840;
liquid_dim = [248.25/3.5; 117.9/3.5; 86.46/3.5];
yatoms = 37;
zatoms = 3*8;
xatoms=liquid_tol/yatoms/zatoms;
liquid_latt = zeros(1,3);
liquid_latt(1) = liquid_dim(1)/(xatoms);
liquid_latt(2) = liquid_dim(2)/(yatoms);
liquid_latt(3) = liquid_dim(3)/(zatoms);
liquid_dim(1) = liquid_latt(1)*xatoms;
%liquid_start = [250; 0; 3*3.93];
liquid_start = [liquid_latt(1)/2; liquid_latt(2)/2; liquid_latt(3)/2];

% create and write positions
m=0;
nn=xatoms*yatoms*zatoms;
posx=zeros(nn,1);
posy=zeros(nn,1);
posz=zeros(nn,1);
for k=0:1:zatoms-1
    if mod(k,2) == 0
        for j=0:1:yatoms-1
            if mod(j,2) == 0
                for i=0:1:xatoms-1
                    m=m+1;
                    posx(m)=liquid_start(1) + i*liquid_latt(1);
                    posy(m)=liquid_start(2) + j*liquid_latt(2);
                    posz(m)=liquid_start(3) + k*liquid_latt(3);
                end
            else
                for i=xatoms-1:-1:0
                    m=m+1;
                    posx(m)=liquid_start(1) + i*liquid_latt(1);
                    posy(m)=liquid_start(2) + j*liquid_latt(2);
                    posz(m)=liquid_start(3) + k*liquid_latt(3);
                end
            end
        end
    else
        for j=yatoms-1:-1:0
            if mod(j,2) == 0
                for i=0:1:xatoms-1
                    m=m+1;
                    posx(m)=liquid_start(1) + i*liquid_latt(1);
                    posy(m)=liquid_start(2) + j*liquid_latt(2);
                    posz(m)=liquid_start(3) + k*liquid_latt(3);
                end
            else
                for i=xatoms-1:-1:0
                    m=m+1;
                    posx(m)=liquid_start(1) + i*liquid_latt(1);
                    posy(m)=liquid_start(2) + j*liquid_latt(2);
                    posz(m)=liquid_start(3) + k*liquid_latt(3);
                end
            end
        end
    end
end
% figure(618)
% plot3(posx,posy,posz,'.');
line2=zeros(nn,1)+1;
line3=zeros(nn,1)+1;
m=0;
npermole = 8;
nmole = nn/npermole;
for i=1:nmole
    for j=1:npermole
        m=m+1;
        line2(m)= i;
    end
end

line1=1:1:nn;
lineb=zeros(nn,1);
% lineall=[line1' line2 line3 posx posy posz lineb lineb lineb];
lineall = [line1' line2 line3 posx posy posz];
fileID = fopen('atoms.dat','w');
% fprintf(fileID,'%i %i %i %f %f %f %i %i %i\n',lineall');
fprintf(fileID,'%i %i %i %f %f %f\n',lineall');
fclose(fileID);

% create liquid bonds and write
nbound=nn/npermole*(npermole-1);
line1=1:1:nbound;
line2=zeros(nbound,1)+1;
line3=zeros(nbound,1);
m=0;
line3temp=1:nn;
for i=1:nn
    if mod(line3temp(i),npermole)~=0
       m=m+1;
       line3(m)=line3temp(i);
    end
end
line4=line3+1;
lineall2=[line1' line2 line3 line4];

fileID = fopen('bonds.dat','w');
fprintf(fileID,'%i %i %i %i\n',lineall2');
fclose(fileID);

% %% Walls
% wall_tol = 16275;
% walls_tol = wall_tol*2;
% latt = 3.93;
% xatoms = 175;
% yatoms = 31;
% zatoms = 3;
% bot_wall_start = [0; 0; 0];
% top_wall_start = [0; 0; 102.18];
% 
% posx = zeros(walls_tol,1);
% posy = zeros(walls_tol,1);
% posz = zeros(walls_tol,1);
% m=0;
% for k=0:1:zatoms-1
%     for j=0:1:yatoms-1
%         for i=0:1:xatoms-1
%             m=m+1;
%             posx(m)=bot_wall_start(1) + i*latt;
%             posy(m)=bot_wall_start(2) + j*latt;
%             posz(m)=bot_wall_start(3) + k*latt;
%         end
%     end
% end
% for k=0:1:zatoms-1
%     for j=0:1:yatoms-1
%         for i=0:1:xatoms-1
%             m=m+1;
%             posx(m)=top_wall_start(1) + i*latt;
%             posy(m)=top_wall_start(2) + j*latt;
%             posz(m)=top_wall_start(3) + k*latt;
%         end
%     end
% end
% line1 = liquid_tol+1:1:walls_tol+liquid_tol;
% line2 = nmole+1:1:nmole+walls_tol;
% line3 = zeros(walls_tol,1)+2;
% lineall3 = [line1' line2' line3 posx posy posz];
% fileID = fopen('atoms.dat','at');
% fprintf(fileID,'%i %i %i %f %f %f\n',lineall3');
% fclose(fileID);
% 
% 
% 
