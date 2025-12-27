import {CommonModule, NgClass} from '@angular/common';
import {Component, OnInit, ViewChild} from '@angular/core';
import {FormsModule, NgForm} from '@angular/forms';
import Swal from 'sweetalert2';
import {ChuDe} from '../../../../models/chude';
import {BoCauHoiResponse} from '../../../../responses/bocauhoi/bocauhoi-response';
import {ResponseObject} from '../../../../responses/response-object';
import {Base} from '../../../base/base';

@Component({
  selector: 'app-admin-bo-cau-hoi-edit',
  standalone: true,
  imports: [CommonModule, FormsModule, NgClass],
  templateUrl: './admin-bo-cau-hoi-edit.html',
  styleUrl: './admin-bo-cau-hoi-edit.scss',
})
export class AdminBoCauHoiEdit extends Base implements OnInit {
  @ViewChild('editForm') editForm!: NgForm;

  bo_cau_hoi_id!: number;
  dto: any = {
    tieu_de: '',
    mo_ta: '',
    chu_de_id: 0,
    che_do_hien_thi: 'PRIVATE',
    loai_su_dung: 'RANKED_ONLY',
    muon_tao_tra_phi: false,
  };

  loading = false;
  saving = false;
  chu_des: ChuDe[] = [];

  readonly loaiSuDungOptions = [
    {value: 'RANKED_ONLY', label: 'Ranked Only (Thi đấu xếp hạng)', icon: 'fa-trophy'},
    {value: 'CASUAL_ONLY', label: 'Casual Only (Đấu vui)', icon: 'fa-gamepad'},
    {value: 'PRACTICE_ONLY', label: 'Practice Only (Chỉ luyện tập)', icon: 'fa-book'},
    {
      value: 'COURSE_ONLY',
      label: 'Course Only (Chỉ dùng cho khóa học)',
      icon: 'fa-graduation-cap',
    },
  ];

  ngOnInit(): void {
    this.bo_cau_hoi_id = Number(this.route.snapshot.paramMap.get('id'));
    this.fetchChuDes();
    this.fetchBoCauHoi();
  }

  fetchChuDes(): void {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<any>) => {
        this.chu_des = res.data || [];
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải danh sách chủ đề', 'error').then((r) => {
        });
      },
    });
  }

  fetchBoCauHoi(): void {
    this.loading = true;
    this.bocauHoiService.getById(this.bo_cau_hoi_id).subscribe({
      next: (res: ResponseObject<BoCauHoiResponse>) => {
        const d = res.data!;
        this.dto = {
          tieu_de: d.tieu_de,
          mo_ta: d.mo_ta,
          chu_de_id: d.chu_de_id,
          che_do_hien_thi: d.che_do_hien_thi,
          loai_su_dung: d.loai_su_dung || 'RANKED_ONLY',
          muon_tao_tra_phi: d.muon_tao_tra_phi || false,
        };
        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tải dữ liệu bộ câu hỏi', 'error').then(() => {
          this.router.navigate(['/admin/bo-cau-hoi']).then();
        });
      },
    });
  }

  onSubmit(form: NgForm): void {
    if (form.invalid) {
      Swal.fire('Cảnh báo', 'Vui lòng nhập đầy đủ thông tin', 'warning').then((r) => {
      });
      return;
    }

    this.saving = true;
    this.bocauHoiService.update(this.bo_cau_hoi_id, this.dto).subscribe({
      next: () => {
        Swal.fire('Thành công', 'Cập nhật bộ câu hỏi thành công', 'success').then(() => {
          this.router.navigate(['/admin/bo-cau-hoi', this.bo_cau_hoi_id]).then();
        });
      },
      error: (err) => {
        Swal.fire('Lỗi', err.error?.message || 'Không thể cập nhật bộ câu hỏi', 'error').then(
          (r) => {
          }
        );
      },
      complete: () => (this.saving = false),
    });
  }

  cancel(): void {
    this.router.navigate(['/admin/bo-cau-hoi', this.bo_cau_hoi_id]).then();
  }

  getIconColor(type: string): string {
    switch (type) {
      case 'RANKED_ONLY':
        return 'purple';
      case 'CASUAL_ONLY':
        return 'blue';
      case 'PRACTICE_ONLY':
        return 'green';
      case 'COURSE_ONLY':
        return 'orange';
      default:
        return 'blue';
    }
  }

  getUsageDesc(type: string): string {
    switch (type) {
      case 'RANKED_ONLY':
        return 'Dùng cho thi đấu xếp hạng chính thức (Official).';
      case 'CASUAL_ONLY':
        return 'Dùng cho đấu giải trí, giao hữu không tính điểm.';
      case 'PRACTICE_ONLY':
        return 'Chỉ dùng để luyện tập cá nhân.';
      case 'COURSE_ONLY':
        return 'Dành riêng cho bài kiểm tra trong khóa học.';
      default:
        return '';
    }
  }
}
