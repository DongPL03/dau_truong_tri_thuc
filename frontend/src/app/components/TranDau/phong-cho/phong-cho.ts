import {Component, OnDestroy, OnInit} from '@angular/core';
import {Base} from '../../base/base';
import {TranDauResponse} from '../../../responses/trandau/trandau-response';
import {ThamGiaTranDauDTO} from '../../../dtos/tran-dau/thamgiatrandau-dto';
import Swal from 'sweetalert2';
import {ResponseObject} from '../../../responses/response-object';
import {PageResponse} from '../../../responses/page-response';

@Component({
  selector: 'app-phong-cho',
  imports: [],
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


  ngOnInit(): void {
    this.load();
  }

  ngOnDestroy(): void {
  }


  load() {
    this.loading = true;
    this.tranDauService.getPendingBattles(this.page, this.size).subscribe({
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
    this.load();
  }


  async tryJoin(room: TranDauResponse) {
    let maPin: string | null = null;
    if (!room.cong_khai) {
      const res = await Swal.fire({
        title: 'Nhập mã PIN', input: 'text', inputLabel: 'Phòng riêng tư',
        inputPlaceholder: 'Mã PIN…', confirmButtonText: 'Tham gia', showCancelButton: true
      });
      if (!res.isConfirmed) return;
      maPin = (res.value || '').trim();
      if (!maPin) {
        await Swal.fire('Thiếu PIN', 'Bạn cần nhập mã PIN', 'warning');
        return;
      }
    }


    const dto: ThamGiaTranDauDTO = {tran_dau_id: room.id, ma_pin: maPin};
    this.tranDauService.joinBattle(dto).subscribe({
      next: () => {
        Swal.fire('Thành công', 'Bạn đã tham gia phòng', 'success');
        this.router.navigate(['/battle/room', room.id]);
      },
      error: (err: { error: { message: string; }; }) => {
        const msg = err?.error?.message || 'Không thể tham gia phòng';
        Swal.fire('Thất bại', msg, 'error');
        console.log(dto);
      }
    });
  }
}
