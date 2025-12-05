import {Component, OnInit} from '@angular/core';
import {FormsModule} from '@angular/forms';
import {CommonModule} from '@angular/common';
import {Base} from '../../../base/base';
import {BoCauHoiResponse} from '../../../../responses/bocauhoi/bocauhoi-response';
import {ChuDe} from '../../../../models/chude';
import {ResponseObject} from '../../../../responses/response-object';
import Swal from 'sweetalert2';
import {PageResponse} from '../../../../responses/page-response';

@Component({
  selector: 'app-admin-bo-cau-hoi-list',
  imports: [CommonModule, FormsModule],
  templateUrl: './admin-bo-cau-hoi-list.html',
  styleUrl: './admin-bo-cau-hoi-list.scss',
  standalone: true
})
export class AdminBoCauHoiList extends Base implements OnInit {

  loading = false;

  // filter
  keyword = '';
  chuDeId: number = 0;
  trangThai: string = 'CHO_DUYET'; // mặc định chỉ xem CHỜ DUYỆT
  sortOrder = 'NEWEST';

  // paging
  page = 0;
  limit = 10;
  totalPages = 0;

  items: BoCauHoiResponse[] = [];
  chuDes: ChuDe[] = [];

  readonly trangThaiOptions = [
    {value: 'CHO_DUYET', label: 'Chờ duyệt'},
    {value: 'DA_DUYET', label: 'Đã duyệt'},
    {value: 'TU_CHOI', label: 'Từ chối'},
    {value: '', label: 'Tất cả'}
  ];

  ngOnInit(): void {
    this.loadChuDe();
    this.loadData();
  }

  loadChuDe(): void {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<ChuDe[]>) => {
        this.chuDes = res.data || [];
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải danh sách chủ đề', 'error').then(r => {
        });
      }
    });
  }

  loadData(): void {
    this.loading = true;
    this.bocauHoiService.getAll(
      this.keyword,
      this.chuDeId,
      '',              // che_do_hien_thi: để backend tự xử lý
      this.trangThai,
      this.sortOrder,
      this.page,
      this.limit
    ).subscribe({
      next: (res: ResponseObject<PageResponse<BoCauHoiResponse>>) => {
        const data = res.data!;
        this.items = data.items ?? [];
        this.totalPages = data.totalPages;
        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tải danh sách bộ câu hỏi', 'error').then(r => {
        });
      }
    });
  }

  /** 🔄 Đổi trang */
  changePage(newPage: number): void {
    if (newPage < 0 || newPage >= this.totalPages) return;
    this.page = newPage;
    this.loadData();
  }

  /** ✅ Duyệt bộ câu hỏi */
  approve(quiz: BoCauHoiResponse): void {
    Swal.fire({
      title: 'Xác nhận duyệt bộ câu hỏi?',
      text: `Bộ: ${quiz.tieu_de}`,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Duyệt',
      cancelButtonText: 'Huỷ'
    }).then(result => {
      if (!result.isConfirmed) return;

      this.bocauHoiService.approveBoCauHoi(quiz.id).subscribe({
        next: (res: ResponseObject<BoCauHoiResponse>) => {
          Swal.fire('Thành công', res.message || 'Đã duyệt bộ câu hỏi', 'success').then(r => {
          });
          this.loadData();
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể duyệt bộ câu hỏi';
          Swal.fire('Lỗi', msg, 'error').then(r => {
          });
        }
      });
    });
  }

  /** ❌ Từ chối bộ câu hỏi */
  reject(quiz: BoCauHoiResponse): void {
    Swal.fire({
      title: 'Lý do từ chối',
      input: 'text',
      inputPlaceholder: 'Nhập lý do từ chối...',
      inputValidator: (value) => {
        if (!value || !value.trim()) {
          return 'Vui lòng nhập lý do từ chối';
        }
        return null;
      },
      showCancelButton: true,
      confirmButtonText: 'Từ chối',
      cancelButtonText: 'Huỷ',
      icon: 'warning'
    }).then(result => {
      if (!result.isConfirmed || !result.value) return;

      const reason = result.value.trim();
      this.bocauHoiService.rejectBoCauHoi(quiz.id, reason).subscribe({
        next: (res: ResponseObject<BoCauHoiResponse>) => {
          Swal.fire('Đã từ chối', res.message || 'Đã từ chối bộ câu hỏi', 'success').then(r => {
          });
          this.loadData();
        },
        error: (err) => {
          const msg = err?.error?.message || 'Không thể từ chối bộ câu hỏi';
          Swal.fire('Lỗi', msg, 'error').then(r => {
          });
        }
      });
    });
  }

  navigateDetail(id: number) {
    // Angular Router tự ghép mảng thành url /admin/bo-cau-hoi/123
    this.router.navigate(['/admin/bo-cau-hoi', id]).then(r => {
    });

  }

  goToCreateAdminBo(): void {
    console.log('Navigate to create new Bo Cau Hoi');
    // this.router.navigate('/admin/bo-cau-hoi/tao-moi').then(r => {
    // });
    this.router.navigate(['/admin/bo-cau-hoi/tao-moi']).then(r => {
    });
  }

}
