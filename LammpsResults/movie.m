bins_data = matfile('densM.mat');
bins = bins_data.bins;

NBX = 37;
NBY = 10;
NBZ = 10;

XD = [130,500];
YD = [0,121.83];
ZD = [11.79,98.0];

timemarks = 100;
rect = [-30, -30, 560, 417];
for i=1:timemarks
    figure(i)
    set(gcf,'position',[5,5,1100,711])
    maxBinCount = max(max(max(bins(i,:,:,:))));   
    isosurface(permute(bins(i,:,:,:),[2,3,4,1]),maxBinCount/2);
    axis([1 NBY,1 NBX,1 NBZ]) %matlab quirk: x<->y
    DX=(XD(2)-XD(1))/NBX;
    DY=(YD(2)-ZD(1))/NBY;
    DZ=(ZD(2)-ZD(1))/NBZ;
    daspect([1/DY 1/DX 1/DZ])
    xlabel('y bin'); 
    ylabel('x bin');
    zlabel('z bin');
    grid
    F(i) = getframe(gcf);
    drawnow
    ax = gcf;
    ax.Units = 'pixels';
    fprintf('%d %d %d %d\n',ax.Position)
    s = size(F(i).cdata);
    fprintf('%d %d\n', s(2), s(1))
    close(figure(i))
end

writerObj = VideoWriter('freeSurface.avi');
writerObj.FrameRate = 1;
writerObj.Quality = 95;

open(writerObj);
for i=1:timemarks
    frame = F(i);
    writeVideo(writerObj, frame);
end
close(writerObj);

