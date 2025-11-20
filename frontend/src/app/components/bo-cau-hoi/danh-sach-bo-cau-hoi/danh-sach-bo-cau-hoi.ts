import {Component, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule} from '@angular/forms';
import Swal from 'sweetalert2';
import {BoCauHoiResponse} from '../../../responses/bocauhoi/bocauhoi-response';
import {ResponseObject} from '../../../responses/response-object';
import {PageResponse} from '../../../responses/page-response';
import {Base} from '../../base/base';
import {ChuDe} from '../../../models/chude';

@Component({
  selector: 'app-bo-cau-hoi-danh-sach-bo-cau-hoi',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './danh-sach-bo-cau-hoi.html',
  styleUrl: './danh-sach-bo-cau-hoi.scss'
})
export class BoCauHoiList extends Base implements OnInit {
  loading = false;
  keyword = '';
  cheDoHienThi = '';
  trangThai = '';
  chuDeId = 0;
  page = 0;
  limit = 1;
  sortOrder = 'NEWEST';
  totalPages = 0;
  currentUserId: number = 0;
  items: BoCauHoiResponse[] = [];

  chuDes: ChuDe[] = [];
  readonly trangThaiOptions = [
    {value: '', label: 'Tất cả'},
    {value: 'DA_DUYET', label: 'Đã duyệt'},
    {value: 'CHO_DUYET', label: 'Chờ duyệt'},
    {value: 'TU_CHOI', label: 'Từ chối'}
  ];

  ngOnInit() {
    this.currentUserId = this.tokenService.getUserId();
    this.loadData();
    this.loadChuDe();
  }

  loadData() {
    this.loading = true;
    this.bocauHoiService.getAll(this.keyword, this.chuDeId, this.cheDoHienThi, this.trangThai, this.sortOrder, this.page, this.limit)
      .subscribe({
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

  loadChuDe() {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<any>) => {
        this.chuDes = res.data || [];
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải danh sách chủ đề', 'error').then(r => {
        });
      }
    });
  }

  /** 🧠 Kiểm tra quyền sửa */
  canEdit(quiz: BoCauHoiResponse): boolean {
    return quiz.nguoi_tao_id === this.currentUserId;
  }

  /** 🧠 Kiểm tra quyền xóa */
  canDelete(quiz: BoCauHoiResponse): boolean {
    return quiz.nguoi_tao_id === this.currentUserId;
  }

  /** ⚠️ Hiện cảnh báo khi không đủ quyền */
  showAccessDeniedAlert(action: string) {
    Swal.fire({
      icon: 'warning',
      title: `Không thể ${action}`,
      text: `Bạn không thể ${action} bộ câu hỏi của người khác hoặc của admin!`,
      confirmButtonColor: '#3085d6',
      confirmButtonText: 'Đã hiểu'
    }).then(r => {
    });
  }


  onSearch() {
    this.page = 0;
    this.loadData();
  }

  applyFilter() {
    this.page = 0;
    this.loadData();
  }

  highlightKeyword(text: string): string {
    if (!this.keyword) return text;
    const regex = new RegExp(`(${this.keyword})`, 'gi');
    return text.replace(regex, '<mark>$1</mark>');
  }


  clearFilter() {
    this.keyword = '';
    this.cheDoHienThi = '';
    this.trangThai = '';
    this.chuDeId = 0;
    this.loadData();
  }

  changePage(p: number) {
    if (p < 0 || p >= this.totalPages) return;
    this.page = p;
    this.loadData();
  }

  goToCreateQuiz() {
    // sau này sẽ điều hướng đến trang tạo bộ câu hỏi
    this.router.navigateByUrl('/bo-cau-hoi/tao-moi-bo-cau-hoi').then(r => {
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

  navigateDetail(id: number) {
    this.router.navigate(['/bo-cau-hoi/chi-tiet-bo-cau-hoi', id]).then(r => {
    });
  }

  confirmDelete(id: number) {
    Swal.fire({
      title: 'Xác nhận xóa?',
      text: 'Bộ câu hỏi này và toàn bộ câu hỏi trong đó sẽ bị xóa vĩnh viễn!',
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Xóa',
      cancelButtonText: 'Hủy',
      confirmButtonColor: '#e11d48'
    }).then(result => {
      if (result.isConfirmed) {
        this.deleteBoCauHoi(id);
      }
    });
  }

  deleteBoCauHoi(id: number) {
    this.bocauHoiService.delete(id).subscribe({
      next: (res) => {
        Swal.fire('Thành công', res.message || 'Xóa thành công', 'success').then(r => {
        });
        this.loadData(); // reload lại danh sách
      },
      error: (err) => {
        Swal.fire('Lỗi', err.error?.message || 'Không thể xóa bộ câu hỏi', 'error').then(r => {
        });
      }
    });
  }

  navigateEdit(id: number) {
    this.router.navigate(['/bo-cau-hoi/sua-bo-cau-hoi', id]).then(r => {
    });
  }
}
