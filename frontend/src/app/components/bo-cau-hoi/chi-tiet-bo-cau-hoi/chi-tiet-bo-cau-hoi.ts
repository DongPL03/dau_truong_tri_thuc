import {Component, OnInit} from '@angular/core';
import {Base} from '../../base/base';
import {CauHoiResponse} from '../../../responses/cauhoi/cauhoi-response';
import {BoCauHoiResponse} from '../../../responses/bocauhoi/bocauhoi-response';
import {ResponseObject} from '../../../responses/response-object';
import {PageResponse} from '../../../responses/page-response';
import Swal from 'sweetalert2';

@Component({
  selector: 'app-chi-tiet-bo-cau-hoi',
  imports: [],
  templateUrl: './chi-tiet-bo-cau-hoi.html',
  styleUrl: './chi-tiet-bo-cau-hoi.scss',
  standalone: true
})
export class BoCauHoiDetail extends Base implements OnInit {
  boCauHoiId!: number;
  bo?: BoCauHoiResponse | null;
  questions: CauHoiResponse[] = [];
  mediaUrl: string = 'assets/images/default-profile-image.jpeg';
  loading = true;
  readonly imageBaseUrl = 'http://localhost:8088/api/v1/cauHoi/media/';


  ngOnInit(): void {
    this.boCauHoiId = Number(this.route.snapshot.paramMap.get('id'));
    this.fetchData();
  }

  fetchData() {
    this.loading = true;

    // Gọi song song 2 API
    this.bocauHoiService.getById(this.boCauHoiId).subscribe({
      next: (res: ResponseObject<BoCauHoiResponse>) => (this.bo = res.data),
      error: () => (this.bo = null),
    });

    this.cauHoiService.getByBoCauHoi(this.boCauHoiId).subscribe({
      next: (res: ResponseObject<PageResponse<CauHoiResponse>>) => {
        this.questions = res.data?.items ?? [];
        // this.mediaUrl = this.imageBaseUrl + this.questions.find(ch => ch.duong_dan_tep)?.duong_dan_tep;
        // console.log(this.mediaUrl);
        this.loading = false;
      },
      error: () => (this.loading = false),
    });
  }

  goCreateQuestion() {
    this.router.navigate(['/bo-cau-hoi', this.boCauHoiId, 'cau-hoi', 'tao-moi-cau-hoi']).then(r => {
    });
  }

  getMediaUrl(q: CauHoiResponse): string {
    if (!q.duong_dan_tep) return '';
    return `${this.imageBaseUrl}${q.duong_dan_tep}`;
  }

  navigateToEdit(id: number): void {
    this.router.navigateByUrl(`/bo-cau-hoi/${this.boCauHoiId}/cau-hoi/${id}/sua-bo-cau-hoi`).then(r => {
    });
  }

  onDeleteQuestion(id: number): void {
    Swal.fire({
      title: 'Xác nhận xóa?',
      text: 'Bạn có chắc muốn xóa câu hỏi này không?',
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Xóa',
      cancelButtonText: 'Hủy',
    }).then(result => {
      if (result.isConfirmed) {
        this.cauHoiService.delete(id).subscribe({
          next: () => {
            Swal.fire('Thành công', 'Xóa câu hỏi thành công', 'success').then(r => {
            });
            this.fetchData(); // reload lại danh sách
          },
          error: () => Swal.fire('Lỗi', 'Không thể xóa câu hỏi', 'error'),
        });
      }
    });
  }

  navigateToDetaiList() {
    this.router.navigateByUrl('/bo-cau-hoi/danh-sach-bo-cau-hoi').then(r => {
    });
  }
}
