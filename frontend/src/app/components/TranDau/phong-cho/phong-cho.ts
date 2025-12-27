import {Component, OnDestroy, OnInit} from '@angular/core';
import {Base} from '../../base/base';
import {TranDauResponse} from '../../../responses/trandau/trandau-response';
import {ThamGiaTranDauDTO} from '../../../dtos/tran-dau/thamgiatrandau-dto';
import Swal from 'sweetalert2';
import {ResponseObject} from '../../../responses/response-object';
import {PageResponse} from '../../../responses/page-response';
import {NgClass} from '@angular/common';

@Component({
  selector: 'app-phong-cho',
  imports: [
    NgClass
  ],
  templateUrl: './phong-cho.html',
  styleUrl: './phong-cho.scss',
  standalone: true
})
export class PhongCho extends Base implements OnInit, OnDestroy {
  loading = false;
  page = 0;
  size = 8;
  totalPages = 0;
  items: TranDauResponse[] = [];
  filter_mode: 'ALL' | 'CASUAL' | 'RANKED' = 'ALL';


  ngOnInit(): void {
    this.loadBattles();
  }

  ngOnDestroy(): void {
  }

  setFilter(mode: 'ALL' | 'CASUAL' | 'RANKED') {
    if (this.filter_mode === mode) return;
    this.filter_mode = mode;
    this.page = 0;
    this.loadBattles();
  }

  loadBattles() {
    this.loading = true;
    const loai = this.filter_mode === 'ALL' ? undefined : this.filter_mode;
    this.tranDauService.getPendingBattles(this.page, this.size, loai).subscribe({
      next: (res: ResponseObject<PageResponse<TranDauResponse>>) => {
        const data = res.data!;
        this.items = data.items || [];
        console.log('Loaded pending battles:', this.items);
        this.totalPages = data.totalPages || 0;
        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không tải được danh sách phòng', 'error').then(r => {
        });
      }
    });
  }


  changePage(p: number) {
    if (p < 0 || p >= this.totalPages) return;
    this.page = p;
    this.loadBattles();
  }


  async tryJoin(room: TranDauResponse) {
    // TRƯỜNG HỢP 1: Phòng công khai -> Vào xem luôn, chưa gọi API tham gia
    if (room.cong_khai) {
      await this.router.navigate(['/tran-dau/phong', room.id]);
      return;
    }

    // TRƯỜNG HỢP 2: Phòng riêng tư -> Nhập PIN -> Gọi API tham gia -> Thành công mới chuyển trang
    const res = await Swal.fire({
      title: 'Nhập mã PIN',
      input: 'text',
      inputLabel: 'Phòng riêng tư',
      inputPlaceholder: 'Mã PIN…',
      confirmButtonText: 'Tham gia',
      showCancelButton: true
    });

    if (!res.isConfirmed) return;

    const maPin = (res.value || '').trim();
    if (!maPin) {
      await Swal.fire('Thiếu PIN', 'Bạn cần nhập mã PIN để vào phòng này', 'warning');
      return;
    }

    const dto: ThamGiaTranDauDTO = {tran_dau_id: room.id, ma_pin: maPin};

    // Gọi API Join
    this.tranDauService.joinBattle(dto).subscribe({
      next: () => {
        Swal.fire({
          icon: 'success',
          title: 'Thành công',
          text: 'Bạn đã tham gia phòng',
          timer: 1500,
          showConfirmButton: false
        });
        // Join xong thì chuyển trang
        this.router.navigate(['/tran-dau/phong', room.id], {
          state: {joined: true}
        });
      },
      error: (err) => {
        const msg = err?.error?.message || 'Mã PIN không đúng hoặc phòng đã đầy';
        Swal.fire('Không thể tham gia', msg, 'error');
      }
    });
  }

  getVisiblePages(): number[] {
    const visible: number[] = [];
    const maxVisible = 7; // số nút trang hiển thị tối đa
    const total = this.totalPages;

    if (total <= maxVisible) {
      return Array.from({length: total}, (_, i) => i);
    }

    const start = Math.max(0, this.page - 3);
    const end = Math.min(total - 1, this.page + 3);

    // luôn hiển thị trang đầu
    if (start > 0) visible.push(0);

    // nếu cách xa đầu -> thêm dấu ...
    if (start > 1) visible.push(-1);

    for (let i = start; i <= end; i++) visible.push(i);

    // nếu cách xa cuối -> thêm dấu ...
    if (end < total - 2) visible.push(-2);

    // luôn hiển thị trang cuối
    if (end < total - 1) visible.push(total - 1);

    return visible;
  }

  goToCreateRoom() {
    this.router.navigateByUrl('/tran-dau/tao-moi-bo-cau-hoi').then(r => {
    });
  }
}
